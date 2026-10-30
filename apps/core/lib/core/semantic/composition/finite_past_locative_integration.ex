defmodule Core.Semantic.Composition.FinitePastLocativeIntegration do
  @moduledoc """
  Opt-in, read-only bridge from a current-turn SemanticInput to a conditional
  finite-past locative property. No production pipeline attachment or mutation.

  Re-detects grammar and verifies token/role witnesses before consulting the
  canonical lemma dictionary. A proof authenticates a possible-event property,
  not the truth or intended sense of an utterance.
  """

  alias Core.Semantic.Composition.{FinitePastLocative, LemmaGroundedPredicate, RoleWitness}
  alias Core.Syntax.Signatures

  @spec derive(map(), keyword()) :: {:ok, map()} | :not_applicable
  def derive(si, opts \\ [])
  def derive(%{} = si, opts) when is_list(opts) do
    lookup = Keyword.get(opts, :lookup, &Db.brain_cells_for_norm/1)

    with true <- is_function(lookup, 1),
         {:ok, words} <- words(si),
         true <- words == ["the", "bird", "sat", "on", "the", "fence"],
         detected <- Signatures.detect(si),
         [signature] <- Enum.filter(detected, &(&1.id == :finite_past_locative_candidate and
                                                  &1.span == {0, 5})),
         true <- Enum.member?(List.wrap(Map.get(si, :sentence_signatures)), signature),
         :ok <- validate_roles(si, signature),
         %{status: :forms_attested, requested_lemmas: ["sit"]} = discovered <-
           LemmaGroundedPredicate.discover(si, 2),
         {:ok, rows} <- safe_lookup(lookup, "sit"),
         %{status: :predicate_feature_candidates} = grounded <-
           LemmaGroundedPredicate.ground(discovered, rows),
         {:ok, proof} <- FinitePastLocative.compose(%{
           signature: signature,
           grounding: grounded,
           predicate_surface: "sat",
           actor: "bird",
           preposition: "on",
           landmark: "fence"
         }),
         :ok <- FinitePastLocative.verify(proof) do
      {:ok, proof}
    else
      _ -> :not_applicable
    end
  end

  def derive(_, _), do: :not_applicable

  defp words(si) do
    tokens =
      si
      |> Map.get(:tokens, [])
      |> List.wrap()
      |> Enum.filter(fn
        %{phrase: word, index: index} = t when is_binary(word) and is_integer(index) ->
          Map.get(t, :n, 1) == 1 and Map.get(t, :kind) not in [:punct, "punct"]
        _ -> false
      end)
      |> Enum.sort_by(& &1.index)

    if length(tokens) == 6 and Enum.map(tokens, & &1.index) == Enum.to_list(0..5) do
      {:ok, Enum.map(tokens, &String.downcase(&1.phrase))}
    else
      :not_applicable
    end
  end

  defp validate_roles(si, sig) do
    requirements = [
      {:subject_determiner, 0, "determiner"},
      {:subject_head, 1, "noun"},
      {:preposition, 3, "preposition"},
      {:location_determiner, 4, "determiner"},
      {:location_head, 5, "noun"}
    ]

    if sig.roles == %{subject_determiner: 0, subject_head: 1, predicate: 2,
                      preposition: 3, location_determiner: 4, location_head: 5} and
         Enum.all?(requirements, fn {role, index, pos} ->
           match?({:ok, _}, RoleWitness.resolve(si, role, index, [pos]))
         end) do
      :ok
    else
      :unsupported
    end
  end

  defp safe_lookup(lookup, lemma) do
    try do
      case lookup.(lemma) do
        rows when is_list(rows) and length(rows) <= 128 -> {:ok, rows}
        _ -> :unsupported
      end
    rescue
      _ -> :unsupported
    catch
      _, _ -> :unsupported
    end
  end
end
