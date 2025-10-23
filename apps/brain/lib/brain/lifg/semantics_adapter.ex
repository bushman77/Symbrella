defmodule Brain.LIFG.SemanticsAdapter do
  @moduledoc """
  Optional semantic adjustments to LIFG scoring and feature prioritization.

  This module allows plug-in enrichment of tokens and senses with semantic
  context — categories, roles, affordances, etc.

  Use to guide disambiguation based on domain, recency, or conceptual proximity.

  Not required; all functions are safely fallback if unused.
  """

  @doc """
  Optionally adjusts the score context before LIFG disambiguation.
  Called by LIFG.Stage1 before lexical scoring completes.

  You can rewrite or tag the `ctx` map to inject semantic cues.

  Keys in `ctx`:
    - :token — the raw word or phrase
    - :sense_id — the candidate sense
    - :base_score — raw lexical score
    - :domain — optional semantic domain or bias
    - :prev_tokens — optional history context

  Returns updated ctx.
  """
  @spec adjust_ctx(map()) :: map()
  def adjust_ctx(%{} = ctx) do
    # Default: no-op — override for semantic enrichment
    ctx
  end

  @doc """
  Optionally tags or annotates a sense with semantic labels.

  You can return:
    - []        → no semantic categories
    - ["body"]  → domain tags
    - ["tool", "noun.artifact"] → WordNet-style or custom

  """
  @spec semantic_tags(term(), term()) :: list(String.t())
  def semantic_tags(_token, _sense_id), do: []

  @doc """
  Experimental: semantic distance function.

  Given two token/sense pairs, return 0.0 (close) to 1.0 (far).

  Used for episodic priming, clustering, or ATL overlap.

  Return nil to disable.
  """
  @spec semantic_distance(map(), map()) :: float() | nil
  def semantic_distance(_ctx1, _ctx2), do: nil
end

