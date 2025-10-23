defmodule Brain.Semantics do
  @moduledoc """
  Semantic anchors used to influence gating decisions in Brain.

  Each entry defines:
  - `:boost` — A small scalar added to gate score (0.0–0.2 suggested)
  - `:regions` — Optional tags indicating which semantic region the word activates

  This module is read-only and safe for use in gating and telemetry.
  """

  @anchors %{
    # Action verbs
    "intend" => %{boost: 0.12, regions: [:lifg]},
    "read"   => %{boost: 0.08, regions: [:pmtg]},
    "know"   => %{boost: 0.10, regions: [:temporal]},
    "remember" => %{boost: 0.11, regions: [:hippocampus]},
    "think" => %{boost: 0.09, regions: [:frontal]},
    "mean"  => %{boost: 0.13, regions: [:atl, :lifg]},

    # Objects + concepts
    "object"   => %{boost: 0.07, regions: [:atl]},
    "person"   => %{boost: 0.06, regions: [:temporal]},
    "story"    => %{boost: 0.09, regions: [:pmtg]},
    "concept"  => %{boost: 0.1,  regions: [:atl]},
    "context"  => %{boost: 0.07, regions: [:temporal]},
    "idea"     => %{boost: 0.08, regions: [:frontal]},
    "event"    => %{boost: 0.05, regions: [:hippocampus]}
  }

  @doc """
  Returns a scalar boost (0.0–0.2) for the given lemma if it's a semantic anchor.
  """
  def bias_for(nil), do: 0.0

  def bias_for(lemma) when is_binary(lemma) do
    Map.get(@anchors, String.downcase(lemma), %{})[:boost] || 0.0
  end

  @doc """
  Returns semantic region tags for the lemma, or [] if not found.
  """
  def regions_for(nil), do: []

  def regions_for(lemma) when is_binary(lemma) do
    Map.get(@anchors, String.downcase(lemma), %{})[:regions] || []
  end
end

