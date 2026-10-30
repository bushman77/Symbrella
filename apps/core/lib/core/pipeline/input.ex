defmodule Core.Pipeline.Input do
  @moduledoc """
  Canonical, non-destructive Phase 1 input preparation.

  One source-aligned scan produces the word tokens and phrase hypotheses used
  by LIFG. Punctuation is retained separately with its original UTF-8 byte
  spans so legacy consumers can continue to index si.tokens by *word*.
  Nothing in this phase establishes a word sense or accepts a correction.
  """

  alias Core.{SemanticInput, Token}
  alias Core.Pipeline.Trace

  @spec prepare(String.t(), keyword()) :: SemanticInput.t()
  def prepare(text, opts \\ []) when is_binary(text) and is_list(opts) do
    max_n = Keyword.get(opts, :max_wordgram_n, 3)

    # Never rewrite the speaker's text. Fuzzy corrections are separate
    # hypotheses; their own evidence is attached by Core.resolve_input/2.
    parsed =
      Token.tokenize(text,
        max_wordgram_n: max_n,
        fuzzy: false,
        span_mode: :chars,
        preserve_source?: true
      )

    case Token.check_span_invariants(parsed) do
      {:ok, _} ->
        {words, punct} =
          parsed.tokens
          |> Enum.split_with(&(Map.get(&1, :kind) == :word))

        words =
          words
          |> Enum.with_index()
          |> Enum.map(fn {token, index} -> %{token | index: index} end)

        phrase_candidates =
          Enum.map(parsed.phrase_candidates, fn
            %{span: {start, stop}} = candidate
            when is_integer(start) and is_integer(stop) and
                   start >= 0 and stop >= start and stop <= byte_size(text) ->
              Map.put(candidate, :surface, binary_part(text, start, stop - start))

            candidate ->
              candidate
          end)

        parsed
        |> Map.put(:sentence, text)
        |> Map.put(:original_text, text)
        |> Map.put(:tokens, words)
        |> Map.put(:punctuation_tokens, punct)
        |> Map.put(:phrase_candidates, phrase_candidates)
        |> Map.put(:tokenization, %{
          version: 1,
          span_unit: :utf8_bytes,
          end_exclusive?: true,
          token_indices: :lexical_words,
          corrections_applied?: false,
          valid?: true
        })
        |> Trace.append(:canonical_tokenization,
          decision: :validated,
          reason: :source_aligned_scan,
          meta: %{
            words: length(words),
            punctuation: length(punct),
            phrase_hypotheses: length(parsed.phrase_candidates)
          }
        )

      {:error, failures} ->
        # Fail closed rather than submitting invalidly aligned senses to LIFG.
        :telemetry.execute(
          [:core, :token, :canonical_span_failure],
          %{count: length(failures)},
          %{stage: :phase1_input}
        )

        parsed
        |> Map.put(:sentence, text)
        |> Map.put(:original_text, text)
        |> Map.put(:tokens, [])
        |> Map.put(:phrase_candidates, [])
        |> Map.put(:punctuation_tokens, [])
        |> Map.put(:tokenization, %{
          version: 1,
          span_unit: :utf8_bytes,
          end_exclusive?: true,
          token_indices: :lexical_words,
          corrections_applied?: false,
          valid?: false,
          failure_count: length(failures)
        })
        |> Trace.append(:canonical_tokenization,
          decision: :invalid_spans,
          reason: :source_alignment_failed,
          meta: %{failures: length(failures)}
        )
    end
  end

  # This is an optional syntactic projection, NOT a second tokenizer.
  # The source-aligned canonical tokens from prepare/2 remain unchanged in
  # the original SI. A possessive suffix can be inspected independently,
  # without inventing a dictionary sense for the whole inflected surface.
  # Contractions with ambiguous auxiliary readings are not expanded here.
  @nonpossessive_bases MapSet.new(~w(he she it that there who what where when why how here let))

  @spec grammatical_projection(SemanticInput.t()) :: SemanticInput.t()
  def grammatical_projection(%SemanticInput{} = si) do
    projected =
      si.tokens
      |> Enum.flat_map(&expand_possessive/1)
      |> Enum.with_index()
      |> Enum.map(fn {token, index} -> %{token | index: index} end)

    tokenization =
      (si.tokenization || %{})
      |> Map.put(:grammatical_projection, :possessive_suffix_candidates)

    %{si | tokens: projected, tokenization: tokenization}
  end

  defp expand_possessive(%Token{phrase: phrase, span: {start, stop}} = token)
       when is_binary(phrase) and is_integer(start) and is_integer(stop) do
    case Regex.run(~r/\A([\p{L}][\p{L}\p{M}-]*)['’]s\z/ui, phrase) do
      [_, base] ->
        if MapSet.member?(@nonpossessive_bases, String.downcase(base)) do
          [token]
        else
          base_end = start + byte_size(base)
          clitic_start = base_end
          clitic = binary_part(phrase, byte_size(base), byte_size(phrase) - byte_size(base))

          [
            %Token{token | phrase: base, norm: String.downcase(base),
              span: {start, base_end}},
            %Token{token | phrase: clitic, norm: String.downcase(clitic),
              span: {clitic_start, stop}, source: :grammatical_possessive_clitic}
          ]
        end

      _ ->
        [token]
    end
  end

  defp expand_possessive(token), do: [token]
end
