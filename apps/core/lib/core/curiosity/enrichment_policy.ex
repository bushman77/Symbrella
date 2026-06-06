defmodule Core.Curiosity.EnrichmentPolicy do
  @moduledoc """
  Pure decisions for Curiosity enrichment results.
  """

  @type action :: {:persist, list()} | :renew_negative_cache

  @spec action_for_enrichment(term()) :: action()
  def action_for_enrichment({:ok, %{"entries" => entries}})
      when is_list(entries) and entries != [] do
    {:persist, entries}
  end

  def action_for_enrichment(_), do: :renew_negative_cache
end
