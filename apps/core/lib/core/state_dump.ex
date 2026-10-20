defmodule Core.StateDump do
  @moduledoc """
  Opt-in, append-only, plain-text snapshots of complete Core runtime terms.

  Uses Inspect with unlimited collection and binary limits and with structs
  expanded as maps. This is a diagnostic text representation of the terms,
  not a lossless serialization format or a filtered telemetry projection.

  Modes:
    * :off   - do not write
    * :final - capture the raw pipeline result, final SemanticInput, and chat reply
    * :all   - additionally capture every instrumented pipeline stage

  Dumps may contain private conversations, session identifiers, lexical
  evidence and memory data. Keep the log local and do not commit it.
  """

  require Logger

  @default_path "priv/symbrella_raw.log"
  @final_stages [:pipeline_raw, :final, :chat_reply]

  @spec capture(term(), atom(), keyword()) :: term()
  def capture(term, stage, opts \\ [])

  def capture(term, stage, opts) when is_atom(stage) and is_list(opts) do
    config = Keyword.get(opts, :config, Application.get_env(:core, __MODULE__, []))

    if enabled_for?(config, stage) do
      try do
        path = config |> Keyword.get(:path, @default_path) |> Path.expand()
        entry = format_entry(stage, term)

        case :global.trans({{__MODULE__, path}, self()}, fn -> append_private(path, entry) end) do
          :ok -> :ok
          {:error, reason} -> Logger.warning("[Core.StateDump] write failed: #{inspect(reason)}")
          {:aborted, reason} -> Logger.warning("[Core.StateDump] lock failed: #{inspect(reason)}")
        end
      rescue
        error ->
          Logger.warning("[Core.StateDump] capture failed: #{Exception.message(error)}")
      end
    end

    term
  end

  def capture(term, _stage, _opts), do: term

  defp enabled_for?(config, stage) when is_list(config) do
    case Keyword.get(config, :mode, :off) do
      :all -> true
      :final -> stage in @final_stages
      _ -> false
    end
  end

  defp enabled_for?(_, _), do: false

  defp format_entry(stage, term) do
    raw =
      inspect(term,
        pretty: true,
        width: 120,
        limit: :infinity,
        printable_limit: :infinity,
        structs: false,
        charlists: :as_lists
      )

    [
      "\n========== SYMBRELLA RAW STATE BEGIN ==========\n",
      "timestamp_utc: ", DateTime.utc_now() |> DateTime.to_iso8601(), "\n",
      "stage: ", Atom.to_string(stage), "\n",
      "writer_pid: ", inspect(self()), "\n",
      "state:\n", raw, "\n",
      "========== SYMBRELLA RAW STATE END ==========\n"
    ]
  end

  defp append_private(path, entry) do
    with :ok <- File.mkdir_p(Path.dirname(path)),
         :ok <- create_if_missing(path),
         :ok <- File.chmod(path, 0o600) do
      File.write(path, entry, [:append])
    end
  end

  defp create_if_missing(path) do
    case File.open(path, [:write, :exclusive]) do
      {:ok, device} -> File.close(device)
      {:error, :eexist} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end
end
