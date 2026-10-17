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

  @doc "Always false: a reviewed migration protocol must be a separate feature."
  def production_write_allowed?(_), do: false

  defp get(row, key) when is_map(row),
    do: Map.get(row, key, Map.get(row, Atom.to_string(key)))
end
