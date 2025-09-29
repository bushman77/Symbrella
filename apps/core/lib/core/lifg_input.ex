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

  # SI → SI
  @spec tokenize(Core.SemanticInput.t()) :: Core.SemanticInput.t()
  def tokenize(%Core.SemanticInput{} = si), do: tokenize(si, [])

  @spec tokenize(Core.SemanticInput.t(), keyword()) :: Core.SemanticInput.t()
  def tokenize(%Core.SemanticInput{} = si, opts) when is_list(opts) do
    tok_opts = tokenizer_defaults()
    tok_out  = Token.tokenize(si, tok_opts)

    {tokens_list, sentence} =
      case tok_out do
        %Core.SemanticInput{tokens: tks, sentence: s} -> {List.wrap(tks), s || Map.get(si, :sentence) || ""}
        tks when is_list(tks)                         -> {tks, Map.get(si, :sentence) || ""}
        _                                             -> {[], Map.get(si, :sentence) || ""}
      end

    cleaned =
      tokens_list
      |> Guard.sanitize()
      |> BoundaryGuard.sanitize(sentence)

    tokens_final =
      case opts do
        [] -> cleaned
        _  -> Core.MWE.Injector.inject(cleaned, opts)
      end

    %Core.SemanticInput{tok_out | tokens: tokens_final}
  end

  # String → [tokens]
  @spec tokenize(String.t()) :: list()
  def tokenize(sentence) when is_binary(sentence) do
    tok_opts = tokenizer_defaults()
    tok_out  = Token.tokenize(sentence, tok_opts)

    {tokens_list, s} =
      case tok_out do
        %Core.SemanticInput{tokens: tks, sentence: ss} -> {List.wrap(tks), ss || sentence}
        tks when is_list(tks)                          -> {tks, sentence}
        _                                              -> {[], sentence}
      end

    tokens_list
    |> Guard.sanitize()
    |> BoundaryGuard.sanitize(s)
  end

  # String → [tokens] then MWE inject
  @spec tokenize(String.t(), keyword()) :: list()
  def tokenize(sentence, opts) when is_list(opts) do
    sentence
    |> tokenize()
    |> Core.MWE.Injector.inject(opts)
  end

  @doc false
  @spec tokenizer_defaults() :: keyword()
  def tokenizer_defaults do
    base = [mode: :words, emit_chargrams: false]
    Keyword.merge(base, Application.get_env(:core, :tokenizer_defaults, []))
  end
end

