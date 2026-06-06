defmodule Core.Curiosity.EnrichmentPolicyTest do
  use ExUnit.Case, async: true

  alias Core.Curiosity.EnrichmentPolicy

  test "persists non-empty enrichment entries" do
    entries = [%{"word" => "example"}]

    assert EnrichmentPolicy.action_for_enrichment({:ok, %{"entries" => entries}}) ==
             {:persist, entries}
  end

  test "renews negative cache for empty or failed enrichment" do
    assert EnrichmentPolicy.action_for_enrichment({:ok, %{"entries" => []}}) ==
             :renew_negative_cache

    assert EnrichmentPolicy.action_for_enrichment({:error, :timeout}) == :renew_negative_cache
    assert EnrichmentPolicy.action_for_enrichment(:undef) == :renew_negative_cache
  end
end
