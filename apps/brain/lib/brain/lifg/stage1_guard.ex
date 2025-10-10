defmodule Brain.LIFG.Stage1.Guard do
  @moduledoc ~S"""
  Stage-1 semantic guard.

  • boundary_ok?/3 — enforce word boundary alignment unless `mw: true`
  • chargram?/2    — detect leaked char-grams (short, misaligned unigram fragments)
  • guard_token/2  — apply both checks and emit telemetry on drops

  Telemetry:
    [:brain, :lifg, :boundary_drop] with %{token_index, span, phrase, mw}
    [:brain, :lifg, :chargram_violation] with %{token_index, span, phrase}
  """

  @type token :: %{
          required(:index) => non_neg_integer(),
          required(:span) => {non_neg_integer(), non_neg_integer()},
          required(:phrase) => String.t(),
          optional(:mw) => boolean(),
          optional(:n) => pos_integer()
        }

  @spec guard_token(map(), token()) :: {:ok, token()} | {:drop, :boundary} | {:drop, :chargram}
  def guard_token(si, %{index: idx, span: span, phrase: phrase} = tok) when is_map(si) do
    sentence = Map.get(si, :sentence) || Map.get(si, "sentence") || ""
    mw? = Map.get(tok, :mw, false)

    if not boundary_ok?(sentence, span, mw?) do
      :telemetry.execute([:brain, :lifg, :boundary_drop], %{}, %{
        token_index: idx,
        span: span,
        phrase: phrase,
        mw: mw?
      })

      {:drop, :boundary}
    else
      if chargram?(sentence, tok) do
        :telemetry.execute([:brain, :lifg, :chargram_violation], %{}, %{
          token_index: idx,
          span: span,
          phrase: phrase
        })

        {:drop, :chargram}
      else
        {:ok, tok}
      end
    end
  end

  @doc "True when span aligns to word boundaries, or `mw` explicitly allows non-boundary."
  @spec boundary_ok?(String.t(), {non_neg_integer(), non_neg_integer()}, boolean()) :: boolean()
  def boundary_ok?(_sentence, _span, true), do: true

  def boundary_ok?(sentence, {s, e}, false) when is_binary(sentence) do
    len = String.length(sentence)
    start_ok? = s == 0 or not letter_or_number?(String.at(sentence, s - 1))
    end_ok? = e >= len or not letter_or_number?(String.at(sentence, e))
    start_ok? and end_ok?
  end

  @doc "Heuristic: short, misaligned unigram fragments inside words."
  @spec chargram?(String.t(), token()) :: boolean()
  def chargram?(sentence, %{span: {s, e}, phrase: phrase, n: n}) do
    unigramish? = (n || 1) == 1 and not String.contains?(phrase, " ")
    misaligned? = not boundary_ok?(sentence, {s, e}, false)
    shortish? = String.length(String.trim(phrase)) <= 2
    unigramish? and misaligned? and shortish?
  end

  def chargram?(sentence, tok),
    do: chargram?(sentence, Map.put_new(tok, :n, 1))

  defp letter_or_number?(nil), do: false
  defp letter_or_number?(ch), do: String.match?(ch, ~r/[\p{L}\p{N}]/u)
end
