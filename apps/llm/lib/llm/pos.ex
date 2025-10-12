# apps/llm/lib/llm/pos.ex
defmodule Llm.Pos do
  @moduledoc false

  alias Llm.{Prompts, Util}
  @pos_tags ~w(
    noun proper_noun verb adjective adverb pronoun determiner
    preposition conjunction interjection numeral auxiliary modal particle
  )

  @pos_call_timeout_default 60_000

  # Public: main entry point (Llm.pos/2 delegates here)
  def run(word, opts) when is_binary(word) do
    w = Util.sanitize_word(word)

    user_opts        = Map.new(Keyword.get(opts, :options, %{}))
    base_options     = Map.merge(Llm.Const.stable_runner_opts(), Map.drop(user_opts, [:num_ctx]))
    keep_alive       = Keyword.get(opts, :keep_alive, Llm.Const.default_keep_alive())
    io_timeout       = Keyword.get(opts, :timeout, @pos_call_timeout_default)
    model            = Keyword.get(opts, :model, nil)
    allow_builtin?   = Keyword.get(opts, :allow_builtin_lexicon, true)
    require_nonempty = Keyword.get(opts, :require_nonempty_syn_ant?, true)

    strong_opts = Util.ensure_min_predict(base_options, 160)

    # 1) JSON pass
    prompt_json =
      [Prompts.pos_system_prompt(), "", "Return ONLY the JSON object.", "word: " <> w]
      |> Enum.join("\n")

    gen_opts_json = [
      format: "json",
      temperature: 0.0,
      options: strong_opts,
      keep_alive: keep_alive,
      timeout: io_timeout
    ]

    result =
      case Llm.generate(model, prompt_json, gen_opts_json) do
        {:ok, %{response: json}} ->
          with {:ok, data}  <- Util.decode_strict_json(json),
               {:ok, data2} <- Util.prefer_exact_lemma(data, w),
               :ok          <- Util.validate_pos_payload(data2, @pos_tags) do
            {:ok, data2}
          else
            _ -> tsv_fallback_generate_then_chat(w, model, strong_opts, keep_alive, allow_builtin?, io_timeout)
          end

        _ ->
          tsv_fallback_generate_then_chat(w, model, strong_opts, keep_alive, allow_builtin?, io_timeout)
      end

    case result do
      {:ok, %{"word" => ^w, "entries" => entries}} ->
        entries2 = ensure_syn_ant_entries(entries, w, model, strong_opts, keep_alive, io_timeout, require_nonempty)
        {:ok, %{"word" => w, "entries" => entries2}}

      other ->
        other
    end
  end

  # TSV fallbacks
  defp tsv_fallback_generate_then_chat(w, model, options, keep_alive, allow_builtin?, io_timeout) do
    prompt =
      Enum.join([
        Prompts.tsv_system_prompt(), "",
        "WORD: #{w}",
        ~s|Return ONLY TSV lines for this word. Lemma must be exactly "#{w}".|
      ], "\n")

    with {:ok, %{response: tsv}} <-
           Llm.generate(model, prompt,
             temperature: 0.0,
             options: Util.ensure_min_predict(options, 160),
             keep_alive: keep_alive,
             timeout: io_timeout
           ),
         entries when entries != [] <- Util.tsv_to_entries(tsv, @pos_tags) |> Util.only_word(w) do
      {:ok, %{"word" => w, "entries" => entries}}
    else
      _ ->
        msgs = [
          %{"role" => "system", "content" => Prompts.tsv_system_prompt()},
          %{"role" => "user", "content" => "WORD: " <> w <>
            ~s|\nReturn TSV lines only. Lemma must be exactly "#{w}". No commentary.|}
        ]

        case Llm.chat(model, msgs,
               temperature: 0.0,
               options: Util.ensure_min_predict(options, 160),
               keep_alive: keep_alive,
               timeout: io_timeout
             ) do
          {:ok, %{content: tsv2}} ->
            entries = Util.tsv_to_entries(tsv2, @pos_tags) |> Util.only_word(w)
            cond do
              entries != [] -> {:ok, %{"word" => w, "entries" => entries}}
              allow_builtin? -> builtin_or_error(w)
              true -> {:error, :tsv_empty}
            end

          _ ->
            if allow_builtin?, do: builtin_or_error(w), else: {:error, :tsv_failed}
        end
    end
  end

  defp builtin_or_error(w) do
    case Llm.Builtin.entries_for(w) do
      [] -> {:error, :tsv_empty}
      entries -> {:ok, %{"word" => w, "entries" => entries}}
    end
  end

  # Enrichment of synonyms/antonyms when missing
  defp ensure_syn_ant_entries(entries, word, model, options, keep_alive, io_timeout, require_nonempty?) do
    Enum.map(entries, fn e ->
      syns = Map.get(e, "synonyms", [])
      ants = Map.get(e, "antonyms", [])

      need? =
        require_nonempty? and
          (not is_list(syns) or syns == [] or not is_list(ants) or ants == [])

      if need? do
        case enrich_syn_ant(word, e, model, options, keep_alive, io_timeout) do
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

  defp enrich_syn_ant(word, entry, model, options, keep_alive, io_timeout) do
    lemma = entry["lemma"] || word
    pos   = entry["pos"] || ""
    gloss = entry["short_gloss"] || ""

    prompt = Prompts.enrichment_prompt(lemma, pos, gloss)

    case Llm.generate(model, prompt,
           format: "json",
           temperature: 0.0,
           options: Llm.Util.ensure_min_predict(options, 64),
           keep_alive: keep_alive,
           timeout: io_timeout
         ) do
      {:ok, %{response: json}} -> Llm.Util.decode_strict_json(json)
      other -> other
    end
  end
end

