# apps/llm/lib/llm/pos.ex
defmodule Llm.Pos do
  @moduledoc """
  Optional POS/enrichment tool.

  Policy:
  - Disabled by default.
  - Must be explicitly enabled via config:
      config :llm, :enable_pos_tool?, true

  Rationale:
  Missing words in BrainCell/DB should be handled by the LLM's generalization and/or
  higher-level pipelines (LIFG/PMTG). We do NOT auto-enrich on misses.
  """

  alias Llm.{Prompts, Util}

  @pos_tags ~w(
    noun proper_noun verb adjective adverb pronoun determiner
    preposition conjunction interjection numeral auxiliary modal particle
  )

  @pos_call_timeout_default 60_000

  @doc """
  Run POS tool for a single word.

  Returns:
    {:ok, %{"word" => word, "entries" => [...]}} | {:error, reason}

  Default:
    {:error, :disabled} unless :enable_pos_tool? is true.
  """
  def run(word, opts) when is_binary(word) do
    if enabled?() do
      do_run(word, opts)
    else
      {:error, :disabled}
    end
  end

  # ────────────────────────────────────────────────────────────────────────────
  # Enabled path (uses ONLY Llm.chat/2; no Llm.generate/3 or Llm.chat/3)
  # ────────────────────────────────────────────────────────────────────────────

  defp do_run(word, opts) do
    w = Util.sanitize_word(word)

    user_opts = Map.new(Keyword.get(opts, :options, %{}))
    base_options = Map.merge(Llm.Const.stable_runner_opts(), Map.drop(user_opts, [:num_ctx]))
    keep_alive = Keyword.get(opts, :keep_alive, Llm.Const.default_keep_alive())
    io_timeout = Keyword.get(opts, :timeout, @pos_call_timeout_default)

    require_nonempty = Keyword.get(opts, :require_nonempty_syn_ant?, true)
    allow_builtin? = Keyword.get(opts, :allow_builtin_lexicon, true)

    strong_opts = Util.ensure_min_predict(base_options, 160)

    # Single JSON pass via chat (OpenAI-compatible)
    prompt_json =
      [Prompts.pos_system_prompt(), "", "Return ONLY the JSON object.", "word: " <> w]
      |> Enum.join("\n")

    msgs = [
      %{"role" => "system", "content" => Prompts.pos_system_prompt()},
      %{"role" => "user", "content" => "Return ONLY the JSON object.\nword: " <> w}
    ]

    chat_opts = [
      temperature: 0.0,
      options: strong_opts,
      keep_alive: keep_alive,
      timeout: io_timeout
    ]

    result =
      case safe_chat(msgs, chat_opts) do
        {:ok, json} ->
          with {:ok, data} <- Util.decode_strict_json(json),
               {:ok, data2} <- Util.prefer_exact_lemma(data, w),
               :ok <- Util.validate_pos_payload(data2, @pos_tags) do
            {:ok, data2}
          else
            _ -> tsv_fallback_chat(w, strong_opts, keep_alive, allow_builtin?, io_timeout)
          end

        _ ->
          tsv_fallback_chat(w, strong_opts, keep_alive, allow_builtin?, io_timeout)
      end

    case result do
      {:ok, %{"word" => ^w, "entries" => entries}} ->
        entries2 =
          ensure_syn_ant_entries(entries, w, strong_opts, keep_alive, io_timeout, require_nonempty)

        {:ok, %{"word" => w, "entries" => entries2}}

      other ->
        other
    end
  end

  # TSV fallback via chat only
  defp tsv_fallback_chat(w, options, keep_alive, allow_builtin?, io_timeout) do
    msgs = [
      %{"role" => "system", "content" => Prompts.tsv_system_prompt()},
      %{
        "role" => "user",
        "content" =>
          "WORD: " <>
            w <>
            ~s|\nReturn TSV lines only. Lemma must be exactly "#{w}". No commentary.|
      }
    ]

    case safe_chat(msgs, temperature: 0.0, options: Util.ensure_min_predict(options, 160), keep_alive: keep_alive, timeout: io_timeout) do
      {:ok, tsv} ->
        entries = Util.tsv_to_entries(tsv, @pos_tags) |> Util.only_word(w)

        cond do
          entries != [] -> {:ok, %{"word" => w, "entries" => entries}}
          allow_builtin? -> builtin_or_error(w)
          true -> {:error, :tsv_empty}
        end

      _ ->
        if allow_builtin?, do: builtin_or_error(w), else: {:error, :tsv_failed}
    end
  end

  defp safe_chat(msgs, opts) do
    # Llm.chat/2 is the supported API in your current baseline
    case Llm.chat(msgs, opts) do
      {:ok, %{content: content}} when is_binary(content) -> {:ok, content}
      {:ok, %{"content" => content}} when is_binary(content) -> {:ok, content}
      other -> other
    end
  rescue
    e -> {:error, {:chat_failed, e}}
  catch
    kind, reason -> {:error, {:chat_failed, {kind, reason}}}
  end

  defp builtin_or_error(w) do
    case Llm.Builtin.entries_for(w) do
      [] -> {:error, :tsv_empty}
      entries -> {:ok, %{"word" => w, "entries" => entries}}
    end
  end

  # Enrichment of synonyms/antonyms when missing
  defp ensure_syn_ant_entries(entries, word, options, keep_alive, io_timeout, require_nonempty?) do
    Enum.map(entries, fn e ->
      syns = Map.get(e, "synonyms", [])
      ants = Map.get(e, "antonyms", [])

      need? =
        require_nonempty? and (not is_list(syns) or syns == [] or not is_list(ants) or ants == [])

      if need? do
        case enrich_syn_ant(word, e, options, keep_alive, io_timeout) do
          {:ok, %{"synonyms" => s2, "antonyms" => a2}} ->
            e
            |> Map.put("synonyms", Llm.Util.sanitize_syn_ant(s2, e["lemma"]))
            |> Map.put("antonyms", Llm.Util.sanitize_syn_ant(a2, e["lemma"]))

          _ ->
            e
            |> Map.put_new("synonyms", syns || [])
            |> Map.put_new("antonyms", ants || [])
        end
      else
        e
        |> Map.put_new("synonyms", syns || [])
        |> Map.put_new("antonyms", ants || [])
      end
    end)
  end

  defp enrich_syn_ant(word, entry, options, keep_alive, io_timeout) do
    lemma = entry["lemma"] || word
    pos = entry["pos"] || ""
    gloss = entry["short_gloss"] || ""

    prompt = Prompts.enrichment_prompt(lemma, pos, gloss)

    msgs = [
      %{"role" => "system", "content" => "Return ONLY JSON."},
      %{"role" => "user", "content" => prompt}
    ]

    case safe_chat(msgs,
           temperature: 0.0,
           options: Llm.Util.ensure_min_predict(options, 64),
           keep_alive: keep_alive,
           timeout: io_timeout
         ) do
      {:ok, json} -> Llm.Util.decode_strict_json(json)
      other -> other
    end
  end

  defp enabled? do
    Application.get_env(:llm, :enable_pos_tool?, false) == true
  end
end
