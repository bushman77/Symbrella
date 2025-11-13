# apps/core/lib/core/curiosity/policy.ex
defmodule Core.Curiosity.Policy do
  @moduledoc """
  Curiosity operating policy: schedules, budgets, and safety rails.
  100% decoupled from user input—purely internal governance.
  """

  @type t :: %__MODULE__{
          interval_ms: pos_integer(),
          jitter_ms: non_neg_integer(),
          batch_size: pos_integer(),
          concurrency: pos_integer(),
          max_tries: pos_integer(),
          llm_model: String.t() | nil,
          token_budget_per_cycle: non_neg_integer()
        }

  # 5 min base cadence
  defstruct interval_ms: 300_000,
            # ± jitter to desync cycles
            jitter_ms: 30_000,
            # phrases per cycle
            batch_size: 16,
            # Termux-friendly
            concurrency: 2,
            # (future) if you add a tries counter
            max_tries: 3,
            # let Llm default unless set
            llm_model: nil,
            # rough soft budget (tokens/effort)
            token_budget_per_cycle: 6_000

  @doc "Load policy from Application env (:core, Core.Curiosity) with sane defaults."
  @spec load() :: t()
  def load do
    cfg = Application.get_env(:core, Core.Curiosity, [])

    %__MODULE__{
      interval_ms: Keyword.get(cfg, :interval_ms, 300_000),
      jitter_ms: Keyword.get(cfg, :jitter_ms, 30_000),
      batch_size: Keyword.get(cfg, :batch_size, 16),
      concurrency: Keyword.get(cfg, :concurrency, 2),
      max_tries: Keyword.get(cfg, :max_tries, 3),
      llm_model: Keyword.get(cfg, :llm_model, nil),
      token_budget_per_cycle: Keyword.get(cfg, :token_budget_per_cycle, 6_000)
    }
  end
end
