defmodule Core.Intent.Matrix do
  @moduledoc """
  Lightweight pattern scoring for intent detection.

  Returns a ranked list of candidates:
    [%{intent: :define, score: 0.82, keyword: "hippocampus", evidence: [...]}, ...]
  """

  alias Core.Intent.Types

  @type token :: String.t()
  @type candidate :: %{
          intent: Types.intent(),
          score: number(),
          keyword: String.t() | nil,
          evidence: [String.t()]
        }

  @greets ~w(hi hello hey yo sup howdy greetings)
  @define_words ~w(define definition meaning what is what's whats)
  @translate_words ~w(translate how do you say how to say)
  @code_markers ~w(mix iex elixir defmodule def fn end alias require import)
  @brain_words ~w(lifg pmtg hippocampus thalamus ofc cerebellum episodic lexicon si lifg_stage)

  # Base weights (override via opts[:weights] if needed)
  @default_w %{
    keyword_exact: 0.60,
    phrase_match: 0.45,
    lexical_hint: 0.30,
    pos_hint: 0.15,
    code_hint: 0.50,
    domain_hint: 0.40
  }

  @spec score([token()], list() | nil, keyword()) :: [candidate()]
  def score(tokens, pos_list \\ nil, opts \\ []) do
    toks = normalize_tokens(tokens)
    w = Map.merge(@default_w, Map.new(Keyword.get(opts, :weights, %{})))
    text = Enum.join(toks, " ")

    cands =
      []
      |> maybe_greet(toks, w)
      |> maybe_define(toks, text, w)
      |> maybe_translate(toks, text, w)
      |> maybe_code(toks, text, w)
      |> maybe_brain_introspect(toks, text, w)
      |> maybe_ask_info_fallback(toks, pos_list, w)

    cands
    |> Enum.group_by(& &1.intent, fn c -> c end)
    |> Enum.map(fn {_intent, cs} ->
      top = Enum.max_by(cs, & &1.score)
      ev = cs |> Enum.flat_map(& &1.evidence) |> Enum.uniq()
      %{top | evidence: ev}
    end)
    |> Enum.sort_by(& &1.score, :desc)
  end

  # ── detectors ───────────────────────────────────────────────────────────────

  defp maybe_greet(acc, toks, w) do
    if Enum.any?(toks, &(&1 in @greets)) do
      kw = Enum.find(toks, &(&1 in @greets))
      score = w.keyword_exact + 0.1
      [%{intent: :greet, score: score, keyword: kw, evidence: ["greet:keyword(#{kw})"]} | acc]
    else
      acc
    end
  end

  defp maybe_define(acc, toks, text, w) do
    cond do
      has_phrase?(text, @define_words) ->
        {kw, ev} = head_noun_hint(toks)
        score = w.phrase_match + if kw, do: 0.15, else: 0.0
        [%{intent: :define, score: score, keyword: kw, evidence: ["define:phrase" | ev]} | acc]

      Enum.any?(toks, &(&1 in @define_words)) ->
        {kw, ev} = head_noun_hint(toks)
        score = w.lexical_hint + if kw, do: 0.15, else: 0.0
        [%{intent: :define, score: score, keyword: kw, evidence: ["define:lex" | ev]} | acc]

      true ->
        acc
    end
  end

  defp maybe_translate(acc, toks, text, w) do
    if has_phrase?(text, @translate_words) or Enum.member?(toks, "translate") do
      {kw, ev} = head_noun_hint(toks)
      score = w.phrase_match + 0.05
      [%{intent: :translate, score: score, keyword: kw, evidence: ["translate:hint" | ev]} | acc]
    else
      acc
    end
  end

  defp maybe_code(acc, toks, text, w) do
    cond do
      has_phrase?(text, ["```", "~H", "defmodule"]) ->
        [
          %{intent: :code, score: w.code_hint + 0.15, keyword: nil, evidence: ["code:block"]}
          | acc
        ]

      Enum.any?(toks, &(&1 in @code_markers)) ->
        [%{intent: :code, score: w.code_hint, keyword: nil, evidence: ["code:marker"]} | acc]

      true ->
        acc
    end
  end

  defp maybe_brain_introspect(acc, toks, text, w) do
    cond do
      has_phrase?(text, ["core.", "brain."]) ->
        [
          %{
            intent: :brain_introspect,
            score: w.domain_hint + 0.2,
            keyword: nil,
            evidence: ["brain:module"]
          }
          | acc
        ]

      Enum.any?(toks, &(&1 in @brain_words)) ->
        [
          %{
            intent: :brain_introspect,
            score: w.domain_hint,
            keyword: nil,
            evidence: ["brain:lex"]
          }
          | acc
        ]

      true ->
        acc
    end
  end

  defp maybe_ask_info_fallback(acc, toks, _pos, w) do
    if Enum.any?(toks, &(&1 in ~w(who what when where why how))) do
      [
        %{
          intent: :ask_info,
          score: w.lexical_hint - 0.05,
          keyword: nil,
          evidence: ["ask_info:wh"]
        }
        | acc
      ]
    else
      acc
    end
  end

  # ── helpers ────────────────────────────────────────────────────────────────

  defp normalize_tokens(nil), do: []
  defp normalize_tokens(toks) when is_list(toks), do: Enum.map(toks, &String.downcase/1)

  defp has_phrase?(text, phrases) do
    Enum.any?(phrases, fn p ->
      p = String.downcase(p)
      String.contains?(text, p)
    end)
  end

  # Super cheap keyword guess: return the first content word after a trigger
  defp head_noun_hint(toks) do
    stop =
      MapSet.new(
        ~w(define definition meaning what is what's whats translate to into in a an the of for from how do you say)
      )

    kw =
      toks
      |> Enum.reject(&MapSet.member?(stop, &1))
      |> Enum.find(fn t -> String.match?(t, ~r/^[a-z][a-z0-9\-_]+$/) end)

    ev = if kw, do: ["keyword(#{kw})"], else: []
    {kw, ev}
  end
end
