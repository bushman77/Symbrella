defmodule Core.Brain.NounTieConceptAudit do
  @moduledoc """
  Read-only evidence audit for exact same-surface noun-sense ties.

  Equal LIFG probabilities establish only ambiguity, not shared meaning.
  The audit inspects REAL noun definitions for the entire tied top slate,
  reports recurring content words and genuine unanimity, and never
  chooses a noun sense, boosts verb candidates or modifies Core state.

  Full SVO, LIFG and ATL agreement is independently gated by
  SynapticExperience.extract/1. Every tied dictionary ID must be
  present exactly once and the selected definition must match its
  currently active BrainCell and LIFG choice.
  """

  import Kernel, except: [inspect: 2]

  alias Core.Brain.SynapticExperience
  alias Core.Semantic.SVOEvent

  @max_tied 16
  @max_inventory 256
  @max_def_bytes 2000
  @tie_epsilon 1.0e-6
  @max_terms_per_gloss 36
  @max_reported_terms 16

  @noise MapSet.new(~w(
    a an and any are as at be been being by can common commonly
    each for from had has have in into is it its kind kinds
    of on one or other others often some such than that the
    their them these this those to typically usually used using
    with who which where when whose means something someone
    noun person persons people type types thing things countable
    uncountable singular plural transitive intransitive
    ambitransitive slang figurative literal
  ))

  @spec inspect(map(), keyword()) :: map()
  def inspect(si, opts \\ [])

  def inspect(%{} = si, opts) when is_list(opts) do
    with {:ok, _} <- SynapticExperience.extract(si),
         %{roles: %{subject_head: actor_i, object_head: patient_i}} <-
           SVOEvent.complete_signature(si) do
      %{
        status: :read_only_audit,
        roles: [
          inspect_role(si, :actor, actor_i, opts),
          inspect_role(si, :patient, patient_i, opts)
        ],
        equal_scores_mean_shared_meaning?: false,
        noun_sense_verified?: false,
        promoted_sense?: false,
        changed_lifg?: false,
        changed_synapses?: false
      }
    else
      _ -> skip(:unvalidated_svo)
    end
  end

  def inspect(_, _), do: skip(:invalid_input)

  defp inspect_role(si, role, index, opts) do
    choices = Enum.filter(List.wrap(Map.get(si, :lifg_choices)),
      &(is_map(&1) and Map.get(&1, :token_index) == index))

    with [choice] <- choices,
         %{id: id, definition: definition, candidate_probs: probs} <- choice,
         true <- is_binary(id) and byte_size(id) <= 128,
         [norm, "noun", suffix] <- String.split(id, "|", parts: 3),
         true <- suffix not in ["", "fallback"],
         true <- valid_probs?(probs) and Map.has_key?(probs, id),
         [selected_cell] <- Enum.filter(List.wrap(Map.get(si, :active_cells)),
           &(is_map(&1) and Map.get(&1, :id) == id)),
         true <- Map.get(selected_cell, :pos) == "noun" and
           Map.get(selected_cell, :norm) == norm and
           Map.get(selected_cell, :definition) == definition,
         {:ok, tied} <- tied_top_ids(probs, norm, id),
         {:ok, rows} <- fetch_rows(norm, opts),
         {:ok, dictionary} <- validate_rows(rows, tied, norm, id, definition) do
      build_report(role, norm, tied, dictionary)
    else
      {:error, reason} -> skip_role(role, reason)
      _ -> skip_role(role, :selected_noun_evidence_unavailable)
    end
  end

  defp valid_probs?(%{} = probs) do
    count = map_size(probs)
    valid_values? =
      Enum.all?(probs, fn {id, v} ->
        is_binary(id) and is_number(v) and v == v and
          v >= 0.0 and v <= 1.0
      end)

    count > 0 and count <= @max_inventory and valid_values? and
      normalized_sum?(probs)
  end

  defp valid_probs?(_), do: false

  defp normalized_sum?(probs) do
    total = Enum.reduce(probs, 0.0, fn {_, p}, acc -> acc + p end)
    total >= 0.98 and total <= 1.02
  end

  defp tied_top_ids(probs, norm, selected_id) do
    selected_score = Map.fetch!(probs, selected_id)

    noun_scores = Enum.filter(probs, fn {candidate_id, _} ->
      case String.split(candidate_id, "|", parts: 3) do
        [^norm, "noun", suffix] -> suffix not in ["", "fallback"]
        _ -> false
      end
    end)

    tied =
      noun_scores
      |> Enum.filter(fn {_, p} -> abs(p - selected_score) <= @tie_epsilon end)
      |> Enum.map(&elem(&1, 0))
      |> Enum.sort()

    cond do
      Enum.any?(noun_scores, fn {_, p} -> p > selected_score + @tie_epsilon end) ->
        {:error, :selected_noun_not_top_scored}

      length(tied) < 2 ->
        {:error, :no_exact_top_tie}

      length(tied) > @max_tied ->
        {:error, :too_many_tied_senses}

      true ->
        {:ok, tied}
    end
  end

  defp fetch_rows(norm, opts) do
    loader = Keyword.get(opts, :fetch_noun_senses, &default_fetch/1)

    if is_function(loader, 1) do
      try do
        case loader.(norm) do
          rows when is_list(rows) and length(rows) <= @max_inventory ->
            {:ok, rows}
          _ -> {:error, :dictionary_unavailable}
        end
      rescue
        _ -> {:error, :dictionary_unavailable}
      catch
        _, _ -> {:error, :dictionary_unavailable}
      end
    else
      {:error, :dictionary_unavailable}
    end
  end

  defp default_fetch(norm) do
    if Code.ensure_loaded?(Db) and function_exported?(Db, :brain_cells_for_norm, 1),
      do: Db.brain_cells_for_norm(norm),
      else: nil
  end

  defp validate_rows(rows, tied, norm, selected_id, definition) do
    selected =
      Enum.filter(rows, fn row ->
        is_map(row) and Map.get(row, :id) in tied and
          Map.get(row, :pos) == "noun" and
          Map.get(row, :norm) == norm
      end)

    ids = Enum.map(selected, &Map.get(&1, :id))

    cond do
      Enum.sort(ids) != tied ->
        {:error, :missing_or_duplicate_dictionary_ids}

      not Enum.all?(selected, fn row ->
        d = Map.get(row, :definition)
        is_binary(d) and d != "" and byte_size(d) <= @max_def_bytes
      end) ->
        {:error, :missing_or_oversized_gloss}

      not Enum.any?(selected, fn row ->
        Map.get(row, :id) == selected_id and
          Map.get(row, :definition) == definition
      end) ->
        {:error, :selected_definition_mismatch}

      true ->
        {:ok, selected}
    end
  end

  defp build_report(role, norm, tied, rows) do
    terms_by_id =
      Map.new(rows, fn row ->
        {row.id, content_tokens(row.definition, norm)}
      end)

    frequencies =
      terms_by_id
      |> Enum.flat_map(fn {id, terms} ->
        Enum.map(terms, &{&1, id})
      end)
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.map(fn {term, ids} ->
        %{term: term, supported_by: Enum.sort(ids),
          count: length(ids)}
      end)
      |> Enum.sort_by(fn x -> {-x.count, x.term} end)

    shared =
      frequencies
      |> Enum.filter(&(&1.count >= 2))
      |> Enum.take(@max_reported_terms)

    unanimous =
      frequencies
      |> Enum.filter(&(&1.count == length(tied)))
      |> Enum.map(& &1.term)
      |> Enum.sort()
      |> Enum.take(@max_reported_terms)

    %{
      role: role,
      status: if(unanimous == [], do: :ambiguous_with_no_unanimous_terms,
        else: :ambiguous_with_unanimous_terms),
      noun_norm: norm,
      tied_noun_sense_ids: tied,
      tied_count: length(tied),
      unanimous_gloss_tokens: unanimous,
      recurrent_but_not_unanimous_gloss_tokens:
        Enum.reject(shared, &(&1.count == length(tied))),
      definitions_compared?: true,
      score_tie_proves_shared_concept?: false,
      exact_noun_sense_verified?: false,
      promoted_sense?: false
    }
  end

  defp content_tokens(gloss, norm) do
    gloss
    |> String.downcase()
    |> String.replace(~r/\A(?:\([^)]{1,120}\)\s*)+/u, "")
    |> String.split(" — ")
    |> List.last()
    |> String.split(~r/[.;:]/u)
    |> hd()
    |> then(&Regex.scan(~r/[\p{L}]{3,26}/u, &1))
    |> List.flatten()
    |> Enum.reject(&(&1 == norm or MapSet.member?(@noise, &1)))
    |> Enum.uniq()
    |> Enum.take(@max_terms_per_gloss)
  end

  defp skip(reason), do: %{
    status: :skipped, reason: reason, changed_lifg?: false,
    changed_synapses?: false, promoted_sense?: false
  }

  defp skip_role(role, reason), do: %{
    role: role, status: :not_auditable, reason: reason,
    exact_noun_sense_verified?: false, promoted_sense?: false
  }
end
