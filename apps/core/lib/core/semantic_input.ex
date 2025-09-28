defmodule Core.SemanticInput do
  @moduledoc """
  Core pipeline carrier (SI). Minimal, unambiguous.

  Fields:
    • sentence      — the original input sentence (single source of truth)
    • source        — origin (e.g., :user, :test)
    • tokens        — tokens produced by Token.tokenize/1
    • active_cells  — lexicon/DB rows attached by downstream stages
    • trace         — ordered list of stage events (maps), including LIFG output

  Notes:
    • We intentionally do NOT include :original_sentence (avoid ambiguity).
    • Stages may read optional fields (e.g., :context_vec) via Map.get/2.
    • LIFG writes its results only into a trace event (no extra struct keys).
  """

  @type t :: %__MODULE__{
          sentence: String.t() | nil,
          source: atom() | nil,
          tokens: [Core.Token.t()],
          active_cells: [map()],
          trace: [map()]
        }

  defstruct sentence: nil,
            source: nil,
            tokens: [],
            active_cells: [],
            trace: []
end
