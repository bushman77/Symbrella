defmodule Core.Recall.Plan do
  @moduledoc """
  Immutable recall execution plan produced by the Recall Gate.

  Fields:
    * triggers     – reasons that caused recall ([:low_conf, :oov_terms_present, ...])
    * budget_ms    – hard time budget for recall IO
    * max_items    – cap on number of results to merge
    * strategies   – ordered retrieval strategies [:exact, :synonym, :embedding]
  """

  @enforce_keys [:triggers, :budget_ms, :max_items, :strategies]
  defstruct triggers: [],
            budget_ms: 40,
            max_items: 8,
            strategies: [:exact, :synonym, :embedding]

  @type strategy :: :exact | :synonym | :embedding

  @type t :: %__MODULE__{
          triggers: [atom()],
          budget_ms: pos_integer(),
          max_items: pos_integer(),
          strategies: [strategy()]
        }
end
