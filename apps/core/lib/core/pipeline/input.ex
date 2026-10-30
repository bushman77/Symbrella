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
end
