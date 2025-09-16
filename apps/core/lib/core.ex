defmodule Core do
  @moduledoc "Sentence → SemanticInput (minimal, Brain-agnostic)."

  alias Core.{SemanticInput, Token, TokenFilters}

  @typedoc """
  Options for resolve_input/2 or /3:
    • :min_n        – int (default 1) for TokenFilters.prune/1
    • :phrase_repo  – module with exists?(phrase) :: boolean (Tokenizer MW detection)
    • :lexicon_mod  – module (default Core.Lexicon.Stage) with run/1 or run/2
    • :gate_mod     – module (default Core.Recall.Gate) exposing gate/2
    • :execute_mod  – module (default Core.Recall.Execute) exposing execute/2
    • :gate_opts    – keyword merged into the gate call
    • :source       – atom source tag (default :core)
  """
  @type opts :: [
          min_n: pos_integer(),
          phrase_repo: module(),
          lexicon_mod: module(),
          gate_mod: module(),
          execute_mod: module(),
          gate_opts: keyword(),
          source: atom()
        ]

  @default_gate_opts [confidence: 0.5, requires_knowledge?: false, oov_terms: []]

  # Public: new canonical entrypoint (opts only)
  @spec resolve_input(String.t()) :: SemanticInput.t()
  def resolve_input(sentence) when is_binary(sentence),
    do: resolve_input(sentence, [])

  @spec resolve_input(String.t(), keyword()) :: SemanticInput.t()
  def resolve_input(sentence, opts) when is_binary(sentence) and is_list(opts) do
    min_n     = Keyword.get(opts, :min_n, 1)
    lexicon   = Keyword.get(opts, :lexicon_mod, Core.Lexicon.Stage)
    gate_mod  = Keyword.get(opts, :gate_mod, Core.Recall.Gate)
    exec_mod  = Keyword.get(opts, :execute_mod, Core.Recall.Execute)
    gate_opts = Keyword.get(opts, :gate_opts, @default_gate_opts)

    sentence
    |> do_tokenize(opts)
    |> TokenFilters.prune(min_n: min_n)
    |> run_lexicon(lexicon, opts)
    |> gate_and_optionally_execute(gate_mod, exec_mod, gate_opts)
  end

  # ── Back-compat shims ─────────────────────────────────────────────────

  # Legacy shape: (sentence, source_atom)
  @spec resolve_input(String.t(), atom()) :: SemanticInput.t()
  def resolve_input(sentence, source) when is_binary(sentence) and is_atom(source) do
    resolve_input(sentence, [source: source])
  end

  # Legacy shape: (sentence, source_atom, opts)
  @spec resolve_input(String.t(), atom(), keyword()) :: SemanticInput.t()
  def resolve_input(sentence, source, opts)
      when is_binary(sentence) and is_atom(source) and is_list(opts) do
    resolve_input(sentence, Keyword.put(opts, :source, source))
  end

  # ── Internals ─────────────────────────────────────────────────────────

  defp do_tokenize(sentence, opts) do
    source = Keyword.get(opts, :source, :core)

    cond do
      function_exported?(Token, :tokenize, 2) ->
        Token.tokenize(sentence, opts)

      function_exported?(Token, :tokenize, 1) ->
        case Token.tokenize(sentence) do
          %SemanticInput{} = si -> %{si | source: source}
          other -> other
        end

      true ->
        %SemanticInput{
          original_sentence: sentence,
          sentence: sentence,
          source: source,
          tokens: []
        }
    end
  end

  defp run_lexicon(si, lexicon_mod, opts) do
    cond do
      function_exported?(lexicon_mod, :run, 2) -> lexicon_mod.run(si, opts)
      function_exported?(lexicon_mod, :run, 1) -> lexicon_mod.run(si)
      true -> si
    end
  end

  defp gate_and_optionally_execute(si, gate_mod, exec_mod, gate_opts) do
    case gate_mod.gate(si, gate_opts) do
      {:skip, si2}       -> si2
      {:plan, plan, si2} -> exec_mod.execute(si2, plan)
    end
  end
end

