defmodule Core.Agency.SemanticReview do
  @moduledoc """
  Prepare blind, dictionary-backed semantic review packets and freeze reviewed
  holdout manifests. Never ask LIFG for a prediction during review preparation.

  A reviewer identifier, vote or SHA-256 checksum is NOT an authenticated
  reviewer signature. Genuine independence, secrecy, and correct lexical
  meaning require external human review and operational controls.
  """

  alias Core.Agency.SemanticHoldout

  @version 1
  @max_cases 128
  @max_senses 512
  @max_id 128
  @max_sentence_bytes 2_048
  @max_definition_bytes 10_000
  @max_development_hashes 4_096

  @spec prepare(map(), (String.t() -> list())) :: {:ok, map()} | {:error, atom()}
  def prepare(%{"dataset_id" => dataset_id, "cases" => cases}, fetch)
      when is_list(cases) and is_function(fetch, 1) do
    cond do
      not valid_id?(dataset_id) ->
        {:error, :invalid_dataset_id}

      cases == [] or length(cases) > @max_cases ->
        {:error, :invalid_case_count}

      true ->
        collect_cases(cases, fetch, dataset_id)
    end
  end

  def prepare(_, _), do: {:error, :invalid_source_cases}

  @doc "A deliberately unfilled reviewer sheet, never an automatic sense label."
  @spec blank_sheet(map()) :: map()
  def blank_sheet(%{"cases" => cases} = packet) when is_list(cases) do
    %{
      "packet_sha256" => packet_sha256(packet),
      "reviewer_id" => "",
      "votes" => Enum.map(cases, fn case_info ->
        %{"case_id" => case_info["case_id"], "sense_id" => nil}
      end)
    }
  end

  def blank_sheet(_), do: %{}

  @doc "Canonical SHA-256 over a frozen packet, not a reviewer authenticity proof."
  @spec packet_sha256(map()) :: String.t()
  def packet_sha256(packet), do: SemanticHoldout.seal(packet)

  @doc """
  Assemble a holdout manifest only when two separate, complete declared
  reviewer sheets agree on every case and all selected IDs exist in the packet.
  A disagreement rejects the batch rather than silently dropping difficult
  cases. The development digests must come from a separately maintained set.
  """
  @spec finalize(map(), map(), map(), list()) :: {:ok, map()} | {:error, atom()}
  def finalize(packet, review_a, review_b, development)
      when is_map(packet) and is_map(review_a) and is_map(review_b) and
           is_list(development) do
    with :ok <- validate_packet(packet),
         :ok <- valid_development(development),
         {:ok, votes_a} <- validate_sheet(packet, review_a),
         {:ok, votes_b} <- validate_sheet(packet, review_b),
         true <- review_a["reviewer_id"] != review_b["reviewer_id"],
         {:ok, manifest} <- assemble(packet, review_a, review_b, votes_a, votes_b, development),
         {:ok, _} <- SemanticHoldout.validate_manifest(manifest) do
      {:ok, manifest}
    else
      false -> {:error, :duplicate_reviewer_identity}
      {:error, reason} -> {:error, reason}
      _ -> {:error, :invalid_reviews}
    end
  end

  def finalize(_, _, _, _), do: {:error, :invalid_arguments}

  @doc """
  Compare a frozen manifest's reviewed dictionary-inventory fingerprints
  against CURRENT Db.BrainCell rows. Called before live benchmark execution.
  """
  @spec verify_current_dictionary(map(), (String.t() -> list())) :: :ok | {:error, atom()}
  def verify_current_dictionary(%{"cases" => cases, "review_inventory" => inventory} = manifest, fetch)
      when is_list(cases) and is_list(inventory) and is_function(fetch, 1) do
    with {:ok, _} <- SemanticHoldout.validate_manifest(manifest),
         true <- length(cases) == length(inventory),
         true <- Enum.zip(cases, inventory) |> Enum.all?(fn {item, entry} ->
           is_map(entry) and item["case_id"] == entry["case_id"] and
             validate_case_input(Map.put(item, "target_word", entry["target_word"])) == :ok
         end) do
      Enum.reduce_while(inventory, :ok, fn entry, _ ->
        word = entry["target_word"]

        with true <- is_binary(word),
             {:ok, senses} <- fetch_snapshot(fetch, word),
             true <- entry["sha256"] == SemanticHoldout.seal(senses) do
          {:cont, :ok}
        else
          _ -> {:halt, {:error, :dictionary_inventory_changed}}
        end
      end)
    else
      _ -> {:error, :invalid_review_inventory}
    end
  end

  def verify_current_dictionary(_, _), do: {:error, :missing_review_inventory}

  defp collect_cases(cases, fetch, dataset_id) do
    result =
      Enum.reduce_while(cases, {:ok, []}, fn item, {:ok, acc} ->
        with :ok <- validate_case_input(item),
             {:ok, senses} <- fetch_snapshot(fetch, item["target_word"]) do
          entry = %{
            "case_id" => item["case_id"],
            "sentence" => item["sentence"],
            "target_index" => item["target_index"],
            "target_word" => item["target_word"],
            "senses" => senses
          }
          {:cont, {:ok, [entry | acc]}}
        else
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    with {:ok, reversed} <- result do
      entries = Enum.reverse(reversed)
      case_ids = Enum.map(entries, & &1["case_id"])
      hashes = Enum.map(entries, &SemanticHoldout.utterance_digest(&1["sentence"]))

      if length(Enum.uniq(case_ids)) != length(case_ids) or
           length(Enum.uniq(hashes)) != length(hashes) do
        {:error, :duplicate_review_case}
      else
        {:ok, %{"version" => @version, "dataset_id" => dataset_id,
                "cases" => entries}}
      end
    end
  end

  defp validate_case_input(%{"case_id" => case_id, "sentence" => sentence,
                              "target_index" => index, "target_word" => word}) do
    cond do
      not valid_id?(case_id) -> {:error, :invalid_case_id}
      not is_binary(sentence) or byte_size(sentence) not in 1..@max_sentence_bytes or
          String.trim(sentence) == "" -> {:error, :invalid_sentence}
      not is_integer(index) or index < 0 or index >= 128 ->
        {:error, :invalid_token_index}
      not is_binary(word) or byte_size(word) not in 1..@max_id or
          word != normalize_word(word) or word == "" ->
        {:error, :invalid_target_word}
      true ->
        case Enum.at(String.split(sentence, ~r/\s+/, trim: true), index) do
          nil -> {:error, :target_index_out_of_bounds}
          token ->
            if normalize_word(token) == word, do: :ok,
              else: {:error, :target_index_word_mismatch}
        end
    end
  end

  defp validate_case_input(_), do: {:error, :invalid_case}

  defp fetch_snapshot(fetch, word) do
    try do
      rows = fetch.(word)

      if is_list(rows) and length(rows) in 2..@max_senses do
        senses = Enum.map(rows, &snapshot_row/1)
        ids = Enum.map(senses, fn
          %{"id" => id} -> id
          _ -> nil
        end)

        if Enum.all?(senses, &valid_row?(&1, word)) and
             length(ids) == length(Enum.uniq(ids)) do
          {:ok, Enum.sort_by(senses, & &1["id"])}
        else
          {:error, :invalid_dictionary_inventory}
        end
      else
        {:error, :insufficient_or_oversized_sense_inventory}
      end
    rescue
      _ -> {:error, :dictionary_lookup_failed}
    catch
      _, _ -> {:error, :dictionary_lookup_failed}
    end
  end

  defp snapshot_row(%{} = row) do
    %{
      "id" => Map.get(row, :id) || Map.get(row, "id"),
      "norm" => Map.get(row, :norm) || Map.get(row, "norm"),
      "pos" => Map.get(row, :pos) || Map.get(row, "pos"),
      "definition" => Map.get(row, :definition) || Map.get(row, "definition"),
      "example" => Map.get(row, :example) || Map.get(row, "example"),
      "status" => Map.get(row, :status) || Map.get(row, "status")
    }
  end

  defp snapshot_row(_), do: nil

  defp valid_row?(%{"id" => id, "norm" => norm, "pos" => pos,
                    "definition" => definition, "example" => example}, word) do
    is_binary(id) and byte_size(id) in 1..160 and
      norm == word and is_binary(pos) and pos != "" and
      (id == word <> "|" <> pos or
         String.starts_with?(id, word <> "|" <> pos <> "|")) and
      (is_nil(definition) or
         (is_binary(definition) and byte_size(definition) <= @max_definition_bytes)) and
      (is_nil(example) or
         (is_binary(example) and byte_size(example) <= @max_definition_bytes))
  end

  defp valid_row?(_, _), do: false

  defp validate_packet(%{"version" => @version, "dataset_id" => dataset,
                          "cases" => cases}) when is_list(cases) do
    cond do
      not valid_id?(dataset) or cases == [] or length(cases) > @max_cases ->
        {:error, :invalid_packet}
      true ->
        result =
          Enum.reduce_while(cases, :ok, fn item, _ ->
            with :ok <- validate_case_input(item),
                 senses when is_list(senses) <- item["senses"],
                 true <- length(senses) in 2..@max_senses,
                 true <- Enum.all?(senses, &valid_row?(&1, item["target_word"])),
                 ids <- Enum.map(senses, & &1["id"]),
                 true <- length(ids) == length(Enum.uniq(ids)),
                 true <- senses == Enum.sort_by(senses, & &1["id"]) do
              {:cont, :ok}
            else
              _ -> {:halt, {:error, :invalid_packet_inventory}}
            end
          end)

        with :ok <- result,
             ids <- Enum.map(cases, & &1["case_id"]),
             hashes <- Enum.map(cases, &SemanticHoldout.utterance_digest(&1["sentence"])),
             true <- length(ids) == length(Enum.uniq(ids)),
             true <- length(hashes) == length(Enum.uniq(hashes)) do
          :ok
        else
          _ -> {:error, :duplicate_or_invalid_packet_cases}
        end
    end
  end

  defp validate_packet(_), do: {:error, :invalid_packet}

  defp valid_development(hashes) do
    if length(hashes) in 1..@max_development_hashes and
         Enum.all?(hashes, &(is_binary(&1) and
           Regex.match?(~r/\A[0-9a-f]{64}\z/, &1))) and
         length(hashes) == length(Enum.uniq(hashes)) do
      :ok
    else
      {:error, :invalid_development_hashes}
    end
  end

  defp validate_sheet(packet, %{"packet_sha256" => hash,
                               "reviewer_id" => reviewer,
                               "votes" => votes})
       when is_list(votes) do
    cases = packet["cases"]
    case_ids = Enum.map(cases, & &1["case_id"])

    cond do
      hash != packet_sha256(packet) ->
        {:error, :review_packet_mismatch}
      not valid_id?(reviewer) or String.starts_with?(reviewer, "REPLACE") ->
        {:error, :invalid_reviewer_identity}
      length(votes) != length(cases) ->
        {:error, :missing_review_votes}
      true ->
        with {:ok, ids} <- vote_map(votes),
             true <- Enum.sort(Map.keys(ids)) == Enum.sort(case_ids),
             true <- Enum.all?(cases, fn item ->
               choice = Map.get(ids, item["case_id"])
               Enum.any?(item["senses"], &(&1["id"] == choice))
             end) do
          {:ok, ids}
        else
          false -> {:error, :invalid_or_missing_review_vote}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  defp validate_sheet(_, _), do: {:error, :invalid_reviewer_sheet}

  defp vote_map(votes) do
    Enum.reduce_while(votes, {:ok, %{}}, fn
      %{"case_id" => case_id, "sense_id" => sense_id}, {:ok, acc}
          when is_binary(case_id) and is_binary(sense_id) ->
        if Map.has_key?(acc, case_id) do
          {:halt, {:error, :duplicate_review_vote}}
        else
          {:cont, {:ok, Map.put(acc, case_id, sense_id)}}
        end

      _, _ ->
        {:halt, {:error, :invalid_review_vote}}
    end)
  end

  defp assemble(packet, review_a, review_b, votes_a, votes_b, development) do
    cases =
      Enum.map(packet["cases"], fn item ->
        id = item["case_id"]
        gold_a = Map.fetch!(votes_a, id)
        gold_b = Map.fetch!(votes_b, id)

        %{
          "case_id" => id,
          "sentence" => item["sentence"],
          "target_index" => item["target_index"],
          "gold_sense_id" => gold_a,
          "split" => "holdout",
          "reviewers" => [
            %{"reviewer_id" => review_a["reviewer_id"], "sense_id" => gold_a},
            %{"reviewer_id" => review_b["reviewer_id"], "sense_id" => gold_b}
          ]
        }
      end)

    if Enum.all?(cases, fn item ->
         [first, second] = item["reviewers"]
         first["sense_id"] == second["sense_id"]
       end) do
      inventory = Enum.map(packet["cases"], fn item ->
        %{
          "case_id" => item["case_id"],
          "target_word" => item["target_word"],
          "sha256" => SemanticHoldout.seal(item["senses"])
        }
      end)

      {:ok, %{
        "version" => @version,
        "dataset_id" => packet["dataset_id"],
        "development_utterance_sha256" => development,
        "review_packet_sha256" => packet_sha256(packet),
        "review_inventory" => inventory,
        "cases" => cases
      }}
    else
      {:error, :reviewer_disagreement}
    end
  end

  defp valid_id?(value),
    do: is_binary(value) and byte_size(value) in 1..@max_id and
      String.trim(value) != ""

  defp normalize_word(word) do
    word
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
  end
end
