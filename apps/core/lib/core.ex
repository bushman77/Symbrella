defmodule Core do
  @moduledoc """
  Central Core pipeline (minimal).
  Sentence in → SemanticInput out.
  """

  alias Core.{SemanticInput, Token, TokenFilters, RuntimeBind, BrainAdapter}
  alias Core.Recall.{Gate, Execute}

  @doc "Resolve a sentence into a SemanticInput (no options, hardcoded defaults)."
  @spec resolve_input(String.t()) :: SemanticInput.t()
  def resolve_input(sentence) when is_binary(sentence) do
    si =
      sentence
      |> tokenize_with_source(:user)     # only two actors; default :user
      |> TokenFilters.prune(min_n: 2)    # simple, fixed pruning
      |> lexicon()                       # guarded call; no-op if stage absent
      |> RuntimeBind.bind(snapshot_fun: &BrainAdapter.snapshot/0)

    case safe_gate(si) do                # Recall Gate kept, but hardcoded + safe
      {:skip, si2} -> si2
      {:plan, plan, si2} -> Execute.execute(si2, plan)
    end
  end

  # ——— helpers (tiny, boring, reliable) ———

  defp tokenize_with_source(sentence, source) do
    si =
      if Code.ensure_loaded?(Token) and function_exported?(Token, :tokenize, 2) do
        Token.tokenize(sentence, source: source)
      else
        Token.tokenize(sentence)
      end

    ensure_source(si, source)
  end

  defp ensure_source(%SemanticInput{source: s} = si, _src) when not is_nil(s), do: si
  defp ensure_source(%SemanticInput{} = si, src), do: %SemanticInput{si | source: src}

  # Guarded call into Core.Lexicon.Stage; returns si unchanged if missing.
  defp lexicon(%SemanticInput{} = si) do
    if Code.ensure_loaded?(Core.Lexicon.Stage) and function_exported?(Core.Lexicon.Stage, :run, 1) do
      Core.Lexicon.Stage.run(si)
    else
      si
    end
  end

  # Hardcoded Recall Gate; never crashes Core if Gate/Execute change or aren’t ready.
  defp safe_gate(%SemanticInput{} = si) do
    if Code.ensure_loaded?(Gate) and function_exported?(Gate, :gate, 2) do
      try do
        Gate.gate(si, confidence: 0.5, requires_knowledge?: false, oov_terms: [])
      rescue
        _ -> {:skip, si}
      catch
        _, _ -> {:skip, si}
      end
    else
      {:skip, si}
    end
  end
end

