defmodule Brain.Synapses.ReceiptStore do
  @moduledoc """
  Private, bounded, authenticated snapshot of individual synaptic experiences.

  This stores one HMAC-keyed receipt per distinct experience, including its
  exact two typed cell edges, provenance class, current review verdict and a
  bounded sequence of attributed review decisions. It never stores source text,
  subject/patient phrases separately, sense glosses, or raw reviewer names.

  The complete snapshot is authenticated with a stable local secret and
  atomically replaced after successful writes; weights are NEVER trusted from
  disk and are rebuilt from validated receipts by Brain.Synapses.Shadow.

  One writer per store directory is required. The secret and receipt snapshot
  must be backed up/restored together; without the secret the snapshot cannot
  be authenticated or replay deduplicated.
  """

  @max_size 4_194_304
  @max_receipts 512
  @max_reviews 8
  @version 1

  @spec open(String.t()) :: {:ok, map()} | {:error, atom()}
  def open(dir) when is_binary(dir) and byte_size(dir) in 1..512 do
    with :ok <- File.mkdir_p(dir),
         :ok <- File.chmod(dir, 0o700),
         {:ok, key} <- secret(dir),
         {:ok, receipts} <- read_snapshot(dir, key) do
      {:ok, %{secret: key, receipts: receipts, dir: dir}}
    else
      _ -> {:error, :store_unavailable_or_invalid}
    end
  end

  def open(_), do: {:error, :invalid_directory}

  @doc "Atomically commit all receipt verdicts; never persist computed weights."
  @spec persist(String.t(), binary(), map()) :: :ok | {:error, atom()}
  def persist(dir, key, receipts)
      when is_binary(dir) and is_binary(key) and byte_size(key) == 32 and
           is_map(receipts) and map_size(receipts) <= @max_receipts do
    with {:ok, records} <- encode_records(receipts),
         {:ok, payload} <- Jason.encode(%{"version" => @version, "receipts" => records}),
         mac <- sign(key, payload),
         {:ok, bytes} <- Jason.encode(%{"payload" => payload, "mac" => mac}),
         true <- byte_size(bytes) <= @max_size,
         :ok <- atomic_write(Path.join(dir, "receipts.json"), bytes) do
      :ok
    else
      _ -> {:error, :persistence_failed}
    end
  end

  def persist(_, _, _), do: {:error, :persistence_failed}

  @spec reviewer_digest(binary(), String.t()) :: String.t()
  def reviewer_digest(key, reviewer),
    do: sign(key, "reviewer:" <> reviewer)

  defp secret(dir) do
    path = Path.join(dir, "secret.key")

    case File.read(path) do
      {:ok, key} when byte_size(key) == 32 ->
        {:ok, key}

      {:error, :enoent} ->
        new_key = :crypto.strong_rand_bytes(32)

        case File.open(path, [:write, :exclusive, :binary]) do
          {:ok, io} ->
            result =
              with :ok <- File.chmod(path, 0o600),
                   :ok <- IO.binwrite(io, new_key),
                   :ok <- :file.sync(io) do
                :ok
              end

            File.close(io)

            case result do
              :ok -> {:ok, new_key}
              _ ->
                _ = File.rm(path)
                {:error, :secret_creation_failed}
            end

          {:error, :eexist} ->
            secret(dir)

          _ ->
            {:error, :secret_creation_failed}
        end

      _ ->
        {:error, :secret_invalid}
    end
  end

  defp read_snapshot(dir, key) do
    path = Path.join(dir, "receipts.json")

    case File.stat(path) do
      {:error, :enoent} ->
        {:ok, %{}}

      {:ok, %{size: size}} when size <= @max_size ->
        with {:ok, bytes} <- File.read(path),
             {:ok, %{"payload" => payload, "mac" => mac}} <- Jason.decode(bytes),
             true <- is_binary(payload) and is_binary(mac),
             {:ok, stored_mac} <- Base.decode16(mac, case: :mixed),
             true <- byte_size(stored_mac) == 32,
             true <- :crypto.hash_equals(stored_mac, :crypto.mac(:hmac, :sha256, key, payload)),
             {:ok, %{"version" => @version, "receipts" => records}} <- Jason.decode(payload),
             true <- is_list(records) and length(records) <= @max_receipts,
             {:ok, receipts} <- decode_records(records) do
          {:ok, receipts}
        else
          _ -> {:error, :invalid_snapshot}
        end

      _ ->
        {:error, :invalid_snapshot}
    end
  end

  defp encode_records(receipts) do
    records =
      receipts
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(fn {hash, record} ->
        %{
          "id" => hash,
          "edges" => Enum.map(record.edges, fn {a, b, rel} -> [a, b, Atom.to_string(rel)] end),
          "provenance" => "user_reported_structural",
          "verdict" => Atom.to_string(record.verdict),
          "reviews" => Enum.map(Map.get(record, :reviews, []), fn x ->
            %{"decision" => Atom.to_string(x.decision),
              "reviewer" => x.reviewer, "at_ms" => x.at_ms}
          end)
        }
      end)

    if Enum.all?(records, &valid_record?/1),
      do: {:ok, records},
      else: {:error, :invalid_receipts}
  end

  defp decode_records(records) do
    Enum.reduce_while(records, {:ok, %{}}, fn record, {:ok, acc} ->
      if valid_record?(record) and not Map.has_key?(acc, record["id"]) do
        [actor, patient] = Enum.map(record["edges"], fn [a, b, rel] ->
          {a, b, String.to_existing_atom(rel)}
        end)

        reviews = Enum.map(record["reviews"], fn event ->
          %{decision: String.to_existing_atom(event["decision"]),
            reviewer: event["reviewer"], at_ms: event["at_ms"]}
        end)

        val = %{edges: [actor, patient],
                verdict: String.to_existing_atom(record["verdict"]),
                reviews: reviews}

        {:cont, {:ok, Map.put(acc, record["id"], val)}}
      else
        {:halt, {:error, :invalid_receipts}}
      end
    end)
  end

  defp valid_record?(%{
         "id" => hash, "edges" => edges, "provenance" => "user_reported_structural",
         "verdict" => verdict, "reviews" => reviews
       })
       when is_binary(hash) and is_list(edges) and is_binary(verdict) and is_list(reviews) do
    is_sha256?(hash) and length(edges) == 2 and
      valid_edge_list?(edges) and
      verdict in ~w(unreviewed confirmed rejected) and
      length(reviews) <= @max_reviews and
      Enum.all?(reviews, &valid_review?/1) and
      compatible_verdict?(verdict, reviews)
  end

  defp valid_record?(_), do: false

  defp compatible_verdict?("unreviewed", []), do: true
  defp compatible_verdict?("unreviewed", _), do: false
  defp compatible_verdict?(verdict, reviews) do
    case List.last(reviews) do
      %{"decision" => last} -> last == verdict
      _ -> false
    end
  end

  defp valid_review?(%{"decision" => decision, "reviewer" => reviewer, "at_ms" => at}) do
    decision in ~w(confirmed rejected) and is_sha256?(reviewer) and
      is_integer(at) and at > 0
  end

  defp valid_review?(_), do: false

  defp valid_edge_list?([[v1, a, "actor"], [v2, p, "patient"]]) do
    valid_cell?(v1, "verb") and valid_cell?(v2, "verb") and v1 == v2 and
      valid_cell?(a, "noun") and valid_cell?(p, "noun")
  end

  defp valid_edge_list?(_), do: false

  defp valid_cell?(id, pos) when is_binary(id) and byte_size(id) in 3..128 do
    case String.split(id, "|") do
      [word, ^pos, sense] ->
        word != "" and sense != "" and sense != "fallback"
      _ -> false
    end
  end

  defp valid_cell?(_, _), do: false

  defp is_sha256?(hash) when is_binary(hash) do
    byte_size(hash) == 64 and
      Regex.match?(~r/\A[0-9a-f]{64}\z/, hash)
  end

  defp is_sha256?(_), do: false

  defp sign(key, bytes) do
    :crypto.mac(:hmac, :sha256, key, bytes)
    |> Base.encode16(case: :lower)
  end

  defp atomic_write(path, bytes) do
    temp = path <> ".new." <> Integer.to_string(System.unique_integer([:positive, :monotonic]))

    case File.open(temp, [:write, :exclusive, :binary]) do
      {:ok, io} ->
        result =
          with :ok <- File.chmod(temp, 0o600),
               :ok <- IO.binwrite(io, bytes),
               :ok <- :file.sync(io) do
            :ok
          end

        File.close(io)

        case result do
          :ok ->
            case File.rename(temp, path) do
              :ok -> :ok
              _ ->
                _ = File.rm(temp)
                {:error, :rename_failed}
            end

          _ ->
            _ = File.rm(temp)
            {:error, :write_failed}
        end

      _ ->
        {:error, :write_failed}
    end
  end
end
