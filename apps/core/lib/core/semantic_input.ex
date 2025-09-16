defmodule Core.SemanticInput do
  @moduledoc """
  Pipeline state used across Core/Brain stages.
  """

  @enforce_keys [:sentence]
  defstruct original_sentence: "",
            sentence: "",
            source: :user,
            tokens: [],
            pos_list: [],
            intent: nil,
            keyword: nil,
            confidence: 0.0,
            mood: nil,
            phrase_matches: [],
            activation_summary: %{},
            pattern_roles: %{},
            active_cells: [],
            brain_state_ref: nil,
            trace: [],
            # Stages results (new)
            running_cell_ids: [],
            db_cells: [],
            negcache_guarded: [],
            mwe_pending: []

  @type t :: %__MODULE__{}
end
