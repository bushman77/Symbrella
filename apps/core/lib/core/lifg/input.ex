# Filename: apps/core/lib/core/lifg/input.ex
defmodule Core.LIFG.Input do
  @moduledoc """
  Canonical entry point for **LIFG-safe tokenization**.

  This module exists to ensure that anything flowing into the LIFG path has:
  - **Char-span metadata** (so slicing, alignment, and boundary logic are reliable)
  - **Stable token indexing** (so candidate selection and scoring can refer to tokens deterministically)
  - **No char-grams in the LIFG path** (unless a caller explicitly opts into a different pipeline)
  - **Word-boundary enforcement** (to prevent substring artifacts from entering sense selection)
  - **Optional MWE injection** (multiword expressions can be inserted when a caller provides an `exists?/1` predicate)

  ## Inputs and outputs

  Accepts:
    * a raw `String.t()` sentence → returns a list of token maps
    * a `%Core.SemanticInput{}` → returns an updated `%Core.SemanticInput{}` with cleaned tokens

  ## Pipeline stages

  1. **Tokenizer (Core.Token)**
     - Tokenize with defaults oriented to LIFG correctness:
       `mode: :words`, `emit_chargrams: false` (unless overridden via config).
     - Force `span_mode: :chars` so spans are anchored to character offsets.

  2. **Span invariant check**
     - Calls `Core.Token.check_span_invariants/1`.
     - On failure, emits telemetry and proceeds defensively with the original tokenization output.

  3. **Guard.sanitize (Brain.LIFG.Guard)**
     - Normalizes tokens into plain maps and ensures indexing/span fields are present
       in the shape expected by the LIFG pipeline.

  4. **BoundaryGuard.sanitize (Brain.LIFG.BoundaryGuard)**
     - Drops char-grams and enforces word-boundary constraints.
     - Always preserves MWEs (multiword tokens) so phrase-level candidates survive boundary filtering.

  5. **Optional MWE injection**
     - If `tokenize/2` is called with non-empty opts, tokens are passed through
       `Core.MWE.Injector.inject/2`.
     - Callers must supply an `exists?/1` predicate if they want real injection;
       otherwise we provide a safe default that returns `false` for all candidates.

  ## Doctest stability

  Tokenizer behavior can be influenced by runtime config via `:core, :tokenizer_defaults`.
  For that reason, doctests in this module assert **minimal invariants** (e.g., "returns a list
  of maps", "returns a SemanticInput struct with a token list") rather than exact token text,
  counts, or spans.

  ## Telemetry

  This module emits:

    * `[:core, :token, :span_invariant_fail]` when `Core.Token.check_span_invariants/1` fails

  Measurements:
    * `%{count: non_neg_integer}` — number of invariant failures

  Metadata:
    * `%{fails: list(), sentence: String.t()}` — failure details and original sentence
  """

  alias Core.Token
  alias Brain.LIFG.{Guard, BoundaryGuard}

  @type token :: map()

  # ───────────────────── Public API: tokenize/1 ─────────────────────

  @doc """
  Tokenize input for the LIFG path, returning **LIFG-safe** tokens.

  This function is intentionally dual-headed:

  - `tokenize(sentence :: String.t())` returns a list of sanitized token maps.
  - `tokenize(si :: Core.SemanticInput.t())` returns a `%Core.SemanticInput{}` whose
    `:tokens` field has been replaced with sanitized tokens.

  ### What “LIFG-safe” means here

  The token stream will be:
  - tokenized using **word mode** (by default)
  - normalized via `Brain.LIFG.Guard.sanitize/1`
  - filtered by `Brain.LIFG.BoundaryGuard.sanitize/2` to prevent char-gram / boundary-violating artifacts

  If span invariants fail, a telemetry event is emitted, and the function continues defensively.

  ## Examples

      iex> tokens = Core.LIFG.Input.tokenize("hello")
      iex> is_list(tokens) and tokens != [] and Enum.all?(tokens, &is_map/1)
      true

      iex> si = Core.LIFG.Input.tokenize(%Core.SemanticInput{sentence: "hello"})
      iex> is_struct(si, Core.SemanticInput) and is_list(si.tokens) and si.tokens != []
      true
  """
  @spec tokenize(String.t()) :: [token]
  @spec tokenize(Core.SemanticInput.t()) :: Core.SemanticInput.t()
  def tokenize(sentence) when is_binary(sentence) do
    tok_opts = tokenizer_defaults()

    # Produce char spans so downstream slicing and boundary checks are reliable
    si0 = Token.tokenize(sentence, Keyword.merge(tok_opts, span_mode: :chars))

    # Invariant check (emit telemetry on failure but proceed defensively)
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

    si1.tokens
    |> Guard.sanitize()
    |> BoundaryGuard.sanitize(si1.sentence)
  end

  def tokenize(%Core.SemanticInput{} = si) do
    tokenize(si, [])
  end

  # ───────────────────── Public API: tokenize/2 ─────────────────────

  @doc """
  Tokenize input for the LIFG path, with an optional **MWE injection pass**.

  This is the opt-in entry point when you want *both*:
  - LIFG-safe tokenization + boundary enforcement, **and**
  - a post-pass that can inject multiword expressions (MWEs) into the token list.

  ## Options

  `opts` are treated as **MWE injection options**:

  - `:exists?` — a required (for real injection) predicate `fun.(phrase :: binary()) :: boolean()`

    If `:exists?` is not supplied or is not a `fun/1`, this module replaces it with
    `&Core.MWE.Injector.default_exists?/1`, which returns `false` for everything.
    This keeps the pipeline safe: injection is effectively disabled, but the call remains valid.

  - All other keys are passed through to `Core.MWE.Injector.inject/2` unchanged.

  ## Return values

  - `tokenize(sentence, opts)` returns a list of token maps.
  - `tokenize(si, opts)` returns a `%Core.SemanticInput{}` with updated tokens.

  ## Examples

      iex> tokens = Core.LIFG.Input.tokenize("hello there", [])
      iex> is_list(tokens) and tokens != [] and Enum.all?(tokens, &is_map/1)
      true

      iex> si = Core.LIFG.Input.tokenize(%Core.SemanticInput{sentence: "hello there"}, [])
      iex> is_struct(si, Core.SemanticInput) and is_list(si.tokens) and si.tokens != []
      true
  """
  @spec tokenize(String.t(), keyword()) :: [token]
  @spec tokenize(Core.SemanticInput.t(), keyword()) :: Core.SemanticInput.t()
  def tokenize(sentence, opts) when is_binary(sentence) and is_list(opts) do
    sentence
    |> tokenize()
    |> maybe_inject_mwes(opts)
  end

  def tokenize(%Core.SemanticInput{} = si, opts) when is_list(opts) do
    tok_opts = tokenizer_defaults()
    sentence = Map.get(si, :sentence) || ""

    si0 = Token.tokenize(sentence, Keyword.merge(tok_opts, span_mode: :chars))

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

    cleaned =
      si1.tokens
      |> Guard.sanitize()
      |> BoundaryGuard.sanitize(si1.sentence)

    tokens_final = maybe_inject_mwes(cleaned, opts)

    %Core.SemanticInput{si1 | tokens: tokens_final}
  end

  # ───────────────────── MWE injection hook ─────────────────────

  # No opts → no injection
  defp maybe_inject_mwes(tokens, []), do: tokens

  # With opts: only inject if we have a sane exists?/1
  defp maybe_inject_mwes(tokens, opts) when is_list(opts) do
    inject_opts =
      case Keyword.get(opts, :exists?) do
        fun when is_function(fun, 1) ->
          # Caller provided a predicate; trust it.
          opts

        _ ->
          # No exists?/1 given:
          #   • We still call Injector, but with default_exists?/1
          #   • That returns false for everything, so no MWEs are added.
          Keyword.put(opts, :exists?, &Core.MWE.Injector.default_exists?/1)
      end

    Core.MWE.Injector.inject(tokens, inject_opts)
  end

  # ───────────────────── Tokenizer defaults ─────────────────────

  @doc """
  Returns the tokenizer defaults used by `tokenize/1` and `tokenize/2`.

  These defaults are biased for the LIFG path:
  - `:mode` defaults to `:words`
  - `:emit_chargrams` defaults to `false`
  - `span_mode: :chars` is forced at call sites for reliable boundary checking

  The returned keyword list is:

  1. A **base** set of safe defaults (defined in this function), merged with
  2. Optional runtime overrides from `Application.get_env(:core, :tokenizer_defaults, [])`.

  This allows test/dev environments to override tokenization settings without changing code.

  ## Examples

      iex> defaults = Core.LIFG.Input.tokenizer_defaults()
      iex> Enum.all?([:mode, :emit_chargrams, :max_wordgram_n], &Keyword.has_key?(defaults, &1))
      true
  """
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
