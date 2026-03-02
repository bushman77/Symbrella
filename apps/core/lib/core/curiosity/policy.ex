# apps/core/lib/core/curiosity/policy.ex
defmodule Core.Curiosity.Policy do
  @moduledoc """
  Curiosity operating policy: schedules, budgets, and safety rails.

  Critical default: **no enrichment**.
  Missing words in BrainCell/DB are not a reason to enrich. The LLM and the
  neuro-symbolic pipeline handle fuzziness without polluting the dictionary.

  If you explicitly want enrichment tooling, you must enable it via config:
      config :core, Core.Curiosity, enable_enrichment?: true

  And separately enable the POS tool (if used):
      config :llm, :enable_pos_tool?, true
  """

  @type t :: %__MODULE__{
          interval_ms: pos_integer(),
          jitter_ms: non_neg_integer(),
          batch_size: pos_integer(),
          concurrency: pos_integer(),
          max_tries: pos_integer(),
          llm_model: String.t() | nil,
          token_budget_per_cycle: non_neg_integer(),
          enable_enrichment?: boolean(),
          enable_llm_pos?: boolean()
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
            token_budget_per_cycle: 6_000,
            # IMPORTANT DEFAULT: no enrichment
            enable_enrichment?: false,
            # Additional gate: even if enrichment is enabled, allow calling Llm.Pos?
            enable_llm_pos?: false

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
      token_budget_per_cycle: Keyword.get(cfg, :token_budget_per_cycle, 6_000),
      enable_enrichment?: Keyword.get(cfg, :enable_enrichment?, false),
      enable_llm_pos?: Keyword.get(cfg, :enable_llm_pos?, false)
    }
  end

  @doc """
  Convenience: should Curiosity attempt any enrichment actions at all?
  """
  @spec enrichment_enabled?(t()) :: boolean()
  def enrichment_enabled?(%__MODULE__{enable_enrichment?: e}), do: e == true

  @doc """
  Convenience: should Curiosity call the optional Llm.Pos tool?
  This is a stricter gate than enable_enrichment? to prevent accidental coupling.
  """
  @spec llm_pos_enabled?(t()) :: boolean()
  def llm_pos_enabled?(%__MODULE__{enable_enrichment?: e, enable_llm_pos?: p}),
    do: e == true and p == true
end
