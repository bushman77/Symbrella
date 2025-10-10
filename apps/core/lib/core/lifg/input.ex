defmodule Core.LIFG.Input do
  @moduledoc """
  Canonical entry point for LIFG-safe tokenization.

  Accepts:
    • a raw String sentence → returns list of tokens
    • a %Core.SemanticInput{} → returns updated SI with cleaned tokens

  Steps:
    - Tokenize with defaults (mode: :words, emit_chargrams: false unless overridden)
    - Guard.sanitize (struct→map, index, spans)
    - BoundaryGuard.sanitize (drop char-grams, enforce word-boundaries; always keep MWEs)
    - (optional) MWE injection with provided opts
  """

  alias Core.Token
  alias Brain.LIFG.{Guard, BoundaryGuard}

  # ---------- Public API ----------

  # String → [tokens]
  @spec tokenize(String.t()) :: list()
  def tokenize(sentence) when is_binary(sentence) do
    tok_opts = tokenizer_defaults()

    # Produce char spans so downstream slicing and boundary checks are reliable
    si0 = Token.tokenize(sentence, Keyword.merge(tok_opts, span_mode: :chars))

    # Invariant check (emit telemetry on failure but proceed defensively)
    si1 =
      case Token.check_span_invariants(si0) do
        {:ok, si_ok} -> si_ok
        {:error, fails} ->
          :telemetry.execute(
            [:core, :token, :span_invariant_fail],
            %{count: length(fails)},
            %{fails: fails, sentence: si0.sentence}
          )

          si0
      end

    si1.tokens
    |> Guard.sanitize()
    |> BoundaryGuard.sanitize(si1.sentence)
  end

  # SI → SI
  @spec tokenize(Core.SemanticInput.t()) :: Core.SemanticInput.t()
  def tokenize(%Core.SemanticInput{} = si), do: tokenize(si, [])

  # SI → SI with opts
  @spec tokenize(Core.SemanticInput.t(), keyword()) :: Core.SemanticInput.t()
  def tokenize(%Core.SemanticInput{} = si, opts) when is_list(opts) do
    tok_opts = tokenizer_defaults()
    sentence = Map.get(si, :sentence) || ""

    si0 = Token.tokenize(sentence, Keyword.merge(tok_opts, span_mode: :chars))

    si1 =
      case Token.check_span_invariants(si0) do
        {:ok, si_ok} -> si_ok
        {:error, fails} ->
          :telemetry.execute(
            [:core, :token, :span_invariant_fail],
            %{count: length(fails)},
            %{fails: fails, sentence: si0.sentence}
          )

          si0
      end

    cleaned =
      si1.tokens
      |> Guard.sanitize()
      |> BoundaryGuard.sanitize(si1.sentence)

    tokens_final =
      case opts do
        [] -> cleaned
        _  -> Core.MWE.Injector.inject(cleaned, opts)
      end

    %Core.SemanticInput{si1 | tokens: tokens_final}
  end

  # String → [tokens] with opts
  @spec tokenize(String.t(), keyword()) :: list()
  def tokenize(sentence, opts) when is_binary(sentence) and is_list(opts) do
    sentence
    |> tokenize()
    |> Core.MWE.Injector.inject(opts)
  end

  @doc false
  @spec tokenizer_defaults() :: keyword()
  def tokenizer_defaults do
    # Base defaults satisfy tests expecting :mode and :emit_chargrams
    base = [
      mode: :words,
      max_wordgram_n: 3,
      emit_chargrams: false,
      lowercase?: true,
      strip_punct?: true
    ]

    env = Application.get_env(:core, :tokenizer_defaults, [])
    Keyword.merge(base, env)
  end
end

