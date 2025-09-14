defmodule Core do
  @moduledoc """
  Central Core pipeline.

  Fast path (DB-free):
    tokenize → prune → bind runtime actives

  Then Recall Gate; if it returns a plan, run bounded Recall Execute.
  """

  alias Core.{SemanticInput, Token, TokenFilters, RuntimeBind, BrainAdapter}
  alias Core.Recall.{Gate, Execute}

  @doc """
  Resolve a sentence into a `SemanticInput` with optional recall.

  Options:
    * `:source`               – defaults to `:user`
    * `:confidence`           – classifier confidence (0..1), default 0.5
    * `:requires_knowledge?`  – boolean, default false
    * `:min_n`                – token frequency cutoff for pruning, default 2
    * `:oov_probe?`           – whether to hit DB to compute OOV; **default false** to avoid DB crashes
  """
  @spec resolve_input(String.t(), keyword()) :: SemanticInput.t()
  def resolve_input(sentence, opts \\ []) when is_binary(sentence) do
    source = Keyword.get(opts, :source, :user)
    conf   = Keyword.get(opts, :confidence, 0.5)
    reqk?  = Keyword.get(opts, :requires_knowledge?, false)
    min_n  = Keyword.get(opts, :min_n, 2)
    oov?   = Keyword.get(opts, :oov_probe?, false)   # <- default false for safety

    si =
      sentence
      |> tokenize_with_source(source)
      |> TokenFilters.prune(min_n: min_n)
      |> RuntimeBind.bind(snapshot_fun: &BrainAdapter.snapshot/0)

    oov_terms = if oov?, do: oov_terms_safe(si), else: []

    case Gate.gate(si, confidence: conf, requires_knowledge?: reqk?, oov_terms: oov_terms) do
      {:skip, si2} -> si2
      {:plan, plan, si2} -> Execute.execute(si2, plan)
    end
  end

  # ---------- helpers ----------

  defp tokenize_with_source(sentence, source) do
    if Code.ensure_loaded?(Token) and function_exported?(Token, :tokenize, 2) do
      apply(Token, :tokenize, [sentence, [source: source]])
    else
      si = apply(Token, :tokenize, [sentence])
      ensure_source(si, source)
    end
  end

  defp ensure_source(%SemanticInput{source: s} = si, _source) when not is_nil(s), do: si
  defp ensure_source(%SemanticInput{} = si, source), do: %SemanticInput{si | source: source}

  # Build an OOV list but **never** crash if DB/migrations aren’t ready.
  defp oov_terms_safe(%SemanticInput{tokens: toks}) do
    exists_fun =
      if Code.ensure_loaded?(Db) and function_exported?(Db, :word_exists?, 1) do
        fn key ->
          try do
            Db.word_exists?(key)
          rescue
            _ -> true   # treat as "exists" → don't trigger OOV recall
          catch
            _, _ -> true
          end
        end
      else
        fn _ -> true end
      end

    toks
    |> Enum.map(fn
      %Token{phrase: p} -> normalize(p)
      %{} = m -> m[:norm] || normalize(m[:phrase] || m[:text] || "")
    end)
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.uniq()
    |> Enum.reject(exists_fun)
  end

  defp normalize(<<>>), do: <<>>
  defp normalize(s) when is_binary(s), do: String.downcase(String.trim(s))
end

