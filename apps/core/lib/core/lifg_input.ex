defmodule Core.LIFG.Input do
  @moduledoc ~S"""
  Canonical entry point for LIFG-safe tokenization.

  Accepts:
    * a raw `String.t()` sentence → returns list of tokens
    * a `%Core.SemanticInput{}` → returns updated SI with cleaned `tokens`

  Steps:
    - Tokenize with defaults (mode: :words, emit_chargrams: false unless overridden)
    - Guard.sanitize (drop char-grams)
    - BoundaryGuard.sanitize (boundary-aligned only)
    - (optional) MWE injection with provided opts
  """

  alias Core.Token
  alias Brain.LIFG.{Guard, BoundaryGuard}

  # ---------- Public API ----------

  # SI → SI
  @spec tokenize(Core.SemanticInput.t()) :: Core.SemanticInput.t()
  def tokenize(%Core.SemanticInput{} = si), do: tokenize(si, [])

  # String → [tokens]
  @spec tokenize(String.t()) :: list()
  def tokenize(sentence) when is_binary(sentence) do
    tok_opts = tokenizer_defaults()

    # ★ Build SI with char spans so downstream slices always match
    # ★
    si0 = Token.tokenize(sentence, Keyword.merge(tok_opts, span_mode: :chars))

    # ★ Invariant check (telemetry on failure, proceed defensively)
    si1 =
      case Token.check_span_invariants(si0) do
        {:ok, si} ->
          si

        {:error, fails} ->
          :telemetry.execute(
            # ★
            [:core, :token, :span_invariant_fail],
            %{count: length(fails)},
            %{fails: fails, sentence: si0.sentence}
          )

          si0
      end

    # Continue your existing pipeline on the *char-span* tokens
    si1.tokens
    |> Guard.sanitize()
    |> BoundaryGuard.sanitize(si1.sentence)
  end

  # SI → SI with opts
  @spec tokenize(Core.SemanticInput.t(), keyword()) :: Core.SemanticInput.t()
  def tokenize(%Core.SemanticInput{} = si, opts) when is_list(opts) do
    tok_opts = tokenizer_defaults()
    sentence = Map.get(si, :sentence) || ""

    # ★ Use the SI's sentence; produce char spans
    # ★
    si0 = Token.tokenize(sentence, Keyword.merge(tok_opts, span_mode: :chars))

    # ★ Invariant check (telemetry on failure)
    si1 =
      case Token.check_span_invariants(si0) do
        {:ok, si_ok} ->
          si_ok

        {:error, fails} ->
          :telemetry.execute(
            # ★
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
        _ -> Core.MWE.Injector.inject(cleaned, opts)
      end

    # ★ Keep the sentence from si1 (normalized) and replace tokens
    # ★
    %Core.SemanticInput{si1 | tokens: tokens_final}
  end

  # String → [tokens] with opts
  @spec tokenize(String.t(), keyword()) :: list()
  def tokenize(sentence, opts) when is_list(opts) do
    sentence
    |> tokenize()
    |> Core.MWE.Injector.inject(opts)
  end

  @doc false
  @spec tokenizer_defaults() :: keyword()
  def tokenizer_defaults do
    # ★ Provide sensible defaults for the new Tokenizer. The legacy keys
    #    (:mode, :emit_chargrams) remain harmless no-ops for compatibility.
    env = Application.get_env(:core, :tokenizer_defaults, [])
    # ★
    [max_wordgram_n: 3, emit_chargrams: false] |> Keyword.merge(env)
  end
end
