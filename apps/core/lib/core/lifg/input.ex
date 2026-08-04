defmodule Core.LIFG.Input do
  @moduledoc """
  Canonical entry point for **LIFG-safe tokenization**.

  Fuzzy text repair is handled by `Core.Token.tokenize/2` — this module does NOT
  do its own fuzzy pass. Pass `fuzzy: false` in opts to disable repair.

  ## Pipeline stages

  1. **Fuzzy repair + Tokenization (Core.Token)** — corrects typos then splits.
  2. **Span invariant check** — emits telemetry on failure, proceeds defensively.
  3. **Guard.sanitize** — normalizes tokens into plain maps.
  4. **BoundaryGuard.sanitize** — drops char-grams, enforces word boundaries.
  5. **Optional MWE injection** — if `:exists?` predicate is provided.
  """

  alias Core.Token
  alias Brain.LIFG.{Guard, BoundaryGuard}

  @type token :: map()

  # ───────────────────── Public API: tokenize/1 ─────────────────────

  @spec tokenize(String.t()) :: [token]
  @spec tokenize(Core.SemanticInput.t()) :: Core.SemanticInput.t()
  def tokenize(sentence) when is_binary(sentence), do: tokenize(sentence, [])

  def tokenize(%Core.SemanticInput{} = si), do: tokenize(si, [])

  # ───────────────────── Public API: tokenize/2 ─────────────────────

  @spec tokenize(String.t(), keyword()) :: [token]
  @spec tokenize(Core.SemanticInput.t(), keyword()) :: Core.SemanticInput.t()

  def tokenize(sentence, opts) when is_binary(sentence) and is_list(opts) do
    si = do_tokenize(sentence, opts)
    maybe_inject_mwes(si.tokens, opts)
  end

  def tokenize(%Core.SemanticInput{} = si, opts) when is_list(opts) do
    original = Map.get(si, :sentence) || ""
    si_tokenized = do_tokenize(original, opts)
    tokens_final = maybe_inject_mwes(si_tokenized.tokens, opts)

    %Core.SemanticInput{si | sentence: si_tokenized.sentence, tokens: tokens_final}
  end

  # ───────────────────── Core tokenization ─────────────────────

  defp do_tokenize(sentence, opts) do
    tok_opts =
      tokenizer_defaults()
      |> Keyword.put(:span_mode, :chars)
      |> Keyword.put(:fuzzy, Keyword.get(opts, :fuzzy, true))

    si0 = Token.tokenize(sentence, tok_opts)

    si1 =
      case Token.check_span_invariants(si0) do
        {:ok, si_ok} ->
          si_ok

        {:error, fails} ->
          :telemetry.execute(
            [:core, :token, :span_invariant_fail],
            %{count: length(fails)},
            %{fails: fails, sentence: si0.sentence}
          )

          si0
      end

    cleaned_tokens =
      si1.tokens
      |> Guard.sanitize()
      |> BoundaryGuard.sanitize(si1.sentence)

    %Core.SemanticInput{si1 | tokens: cleaned_tokens}
  end

  # ───────────────────── MWE injection hook ─────────────────────

  defp maybe_inject_mwes(tokens, []), do: tokens

  defp maybe_inject_mwes(tokens, opts) when is_list(opts) do
    inject_opts =
      case Keyword.get(opts, :exists?) do
        fun when is_function(fun, 1) ->
          opts

        _ ->
          Keyword.put(opts, :exists?, &Core.MWE.Injector.default_exists?/1)
      end

    Core.MWE.Injector.inject(tokens, inject_opts)
  end

  # ───────────────────── Tokenizer defaults ─────────────────────

  @spec tokenizer_defaults() :: keyword()
  def tokenizer_defaults do
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
