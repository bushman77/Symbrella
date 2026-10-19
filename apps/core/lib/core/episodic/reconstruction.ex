defmodule Core.Episodic.Reconstruction do
  @moduledoc """
  Read-only reconstruction audit for ordinary persisted Hippocampal episodes.

  No historical row is written, promoted, backdated or merged into live
  user-reported events. Old rows with unknown roles need attribution review
  before any future migration. Every candidate retains its source row ID,
  session and original timestamp for human inspection.
  """

  alias Core.Semantic.EpisodicEvents

  @spec preview_row(map()) :: map()
  def preview_row(%{} = row) do
    sentence = get(row, :sentence)
    role = get(row, :role)
    source = get(row, :source)
    episode_id = get(row, :id)
    session_id = get(row, :session_id)
    created = get(row, :inserted_at)
    metadata = get(row, :meta) || %{}
    kind = get(metadata, :kind)

    status =
      cond do
        kind in ["user_reported_event", "user_reported_spatial", "fact",
                 :user_reported_event, :user_reported_spatial, :fact] -> :already_structured
        role in ["assistant", "system", "tool", :assistant, :system, :tool] -> :non_user
        not is_binary(sentence) or String.trim(sentence) == "" -> :empty
        role in ["user", :user] and source in [nil, "user", "prod", :user, :prod] -> :attributed_user
        role in [nil, "", "unknown"] -> :unattributed_legacy
        true -> :untrusted_source
      end

    candidates =
      if status in [:attributed_user, :unattributed_legacy] do
        EpisodicEvents.extract(sentence)
      else
        []
      end

    %{
      episode_id: episode_id, session_id: session_id,
      timestamp: created, attribution: status, candidates: candidates,
      candidate_count: length(candidates), write_authorized?: false
    }
  end

  def preview_row(_), do: %{attribution: :invalid, candidates: [],
    candidate_count: 0, write_authorized?: false}

  @doc """
  Detect structured events already representing a nearby ordinary cognitive
  trace, without guessing that all same-session memories are duplicates.

  A match must agree on session, entity, relation, value and chronology.
  Prefer exact original-sentence equality when the typed event retained its
  source sentence. Older typed rows without sentences require a close
  timestamp and ID (the two rows are written in sequence).
  """
  @spec preview_batch([map()]) :: [map()]
  def preview_batch(rows) when is_list(rows) do
    typed =
      Enum.filter(rows, fn row ->
        meta = get(row, :meta) || %{}
        get(meta, :kind) in ["user_reported_event", :user_reported_event] and
          get(meta, :subject) in ["user", :user]
      end)

    Enum.map(rows, fn row ->
      result = preview_row(row)
      associated =
        if result.candidate_count > 0 do
          Enum.find(typed, &covers_trace?(&1, row, result.candidates))
        end

      Map.merge(result, %{
        already_indexed?: is_map(associated),
        matching_structured_episode_id:
          if(is_map(associated), do: get(associated, :id), else: nil)
      })
    end)
  end

  def preview_batch(_), do: []

  defp covers_trace?(typed, ordinary, candidates) do
    meta = get(typed, :meta) || %{}
    old_id = get(ordinary, :id)
    typed_id = get(typed, :id)
    old_at = get(ordinary, :inserted_at)
    typed_at = get(typed, :inserted_at)
    typed_sentence = get(typed, :sentence)
    source_sentence = get(ordinary, :sentence)

    close? =
      is_integer(old_id) and is_integer(typed_id) and
        typed_id < old_id and old_id - typed_id <= 6 and
        match?(%NaiveDateTime{}, old_at) and
        match?(%NaiveDateTime{}, typed_at) and
        NaiveDateTime.diff(old_at, typed_at, :millisecond) in 0..5_000

    same_sentence? =
      not is_binary(typed_sentence) or
        (is_binary(source_sentence) and
           String.trim(typed_sentence) == String.trim(source_sentence))

    close? and same_sentence? and
      get(meta, :session_id) == get(ordinary, :session_id) and
      Enum.any?(candidates, fn event ->
        get(meta, :entity_key) == event.entity_key and
          get(meta, :relation) == event.relation and
          get(meta, :value) == event.value and
          get(meta, :status) == Atom.to_string(event.status)
      end)
  end

  @doc "Always false: a reviewed migration protocol must be a separate feature."
  def production_write_allowed?(_), do: false

  defp get(row, key) when is_map(row),
    do: Map.get(row, key, Map.get(row, Atom.to_string(key)))
end
