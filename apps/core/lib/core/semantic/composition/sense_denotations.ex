defmodule Core.Semantic.Composition.SenseDenotations do
  @moduledoc """
  Read-only, experimental denotation hypotheses extracted from dictionary
  definitions. Supports *multiple* readings per sense and explicit
  selectional requirements. The rules match gloss text, never a sense number.

  These are candidate annotations, NOT verified semantic representations or
  truth claims. Requirements are not yet checked against predicate/participant
  evidence; no result may authorize a LIFG winner or a lambda proof.
  Unknown readings are preserved as unresolved.
  """

  # Rules are narrowly evidenced gloss fragments. They are intentionally
  # incomplete: extending coverage requires separately reviewed examples,
  # negative contrasts, and held-out evaluation.
  defp rules do
    [
    %{family: :spatial_location, subtype: :upper_surface,
      pattern: ~r/\bPositioned at the upper surface of\b/ui,
      requires: [:supporting_surface]},
    %{family: :spatial_location, subtype: :outer_surface_attachment,
      pattern: ~r/\bresting against the outer surface of\b/ui,
      requires: [:physical_surface]},
    %{family: :spatial_location, subtype: :covering,
      pattern: ~r/\bIndicating position or location\.\s*—\s*Covering\b/ui,
      requires: [:coverable_landmark]},
    %{family: :spatial_location, subtype: :geographic_location,
      pattern: ~r/\bgeographical location or position\b/ui,
      requires: [:geographic_landmark]},
    %{family: :spatial_location, subtype: :relative_position,
      pattern: ~r/\brelative spatial position\b/ui,
      requires: [:spatial_reference]},
    %{family: :spatial_location, subtype: :adjacency,
      pattern: ~r/\bNear;\s*adjacent to;\s*alongside\b/ui,
      requires: [:spatial_reference]},
    %{family: :spatial_location, subtype: :aboard_transport,
      pattern: ~r/\bAboard \(a mode of transport\b/ui,
      requires: [:transport_landmark]},
    %{family: :spatial_direction, subtype: :vessel_bearing,
      pattern: ~r/\bIn the direction of \(some part of one's vessel\b/ui,
      requires: [:nautical_reference]},
    %{family: :physical_interaction, subtype: :force_target,
      pattern: ~r/\b(?:application of force|pushing,\s*pulling,\s*pressing)\b/ui,
      requires: [:force_event, :affected_target]},
    %{family: :physical_interaction, subtype: :impact_target,
      pattern: ~r/\b(?:hitting,\s*rubbing,\s*scratching|thing impacted or contacted)\b/ui,
      requires: [:contact_event, :affected_target]},
    %{family: :physical_interaction, subtype: :contact_means,
      pattern: ~r/\bperformance or action by contact with the surface\b/ui,
      requires: [:contact_action]},
    %{family: :instrument, subtype: :means,
      pattern: ~r/\b(?:By means of;\s*with|means or medium)\b/ui,
      requires: [:instrumental_action]},
    %{family: :support, subtype: :self_support,
      pattern: ~r/\bSupported by \(the specified part of itself\)/ui,
      requires: [:body_or_self_part]},
    %{family: :support, subtype: :resting_upon,
      pattern: ~r/\bresting upon\b/ui,
      requires: [:supporting_landmark]},
    %{family: :discourse_topic, subtype: :subject_matter,
      pattern: ~r/\bDealing with the subject of;\s*about;\s*concerning\b/ui,
      requires: [:topic_bearing_predicate]},
    %{family: :opposition, subtype: :adversary,
      pattern: ~r/\bAgainst;\s*in opposition to\b/ui,
      requires: [:adversarial_event]},
    %{family: :invocation, subtype: :imprecation,
      pattern: ~r/\bimprecation or invocation\b/ui,
      requires: [:invocation_context]},
    %{family: :temporal, subtype: :date,
      pattern: ~r/\bdate or day of\b/ui,
      requires: [:date_landmark]},
    %{family: :temporal, subtype: :instant,
      pattern: ~r/\binstant or cusp\b/ui,
      requires: [:time_landmark]},
    %{family: :sequence, subtype: :ordinal_position,
      pattern: ~r/\bposition within a sequence\b/ui,
      requires: [:ordered_sequence]},
    %{family: :membership, subtype: :group_member,
      pattern: ~r/\bServing as a member of\b/ui,
      requires: [:group_landmark]},
    %{family: :possession, subtype: :has_on_person,
      pattern: ~r/\bIn the possession of\b/ui,
      requires: [:possessor]},
    %{family: :emotion, subtype: :experiencer,
      pattern: ~r/\bperson experiencing an emotion\b/ui,
      requires: [:experiencer]},
    %{family: :emotion, subtype: :target,
      pattern: ~r/\bobject of an emotion\b/ui,
      requires: [:emotion_predicate]},
    %{family: :mathematical, subtype: :function_domain,
      pattern: ~r/\b(?:domain and codomain|domain and V as codomain)\b/ui,
      requires: [:mathematical_function]}
    ]
  end

  @spec propose(map()) :: map()
  def propose(%{} = cell) do
    id = field(cell, :id)
    definition = field(cell, :definition)

    hypotheses =
      if is_binary(definition) do
        rules()
        |> Enum.flat_map(fn rule ->
          case Regex.run(rule.pattern, definition) do
            [matched | _] ->
              [%{
                relation_family: rule.family,
                subtype: rule.subtype,
                required_context: rule.requires,
                source: :dictionary_gloss,
                matched_text: matched,
                verification: :not_established
              }]

            _ -> []
          end
        end)
        |> Enum.uniq_by(&{&1.relation_family, &1.subtype})
      else
        []
      end

    %{
      sense_id: id,
      candidates: hypotheses,
      candidate_count: length(hypotheses),
      unresolved?: hypotheses == [],
      multiple_readings?: length(hypotheses) > 1,
      interpreted_word_sense?: false,
      changes_lifg?: false
    }
  end

  def propose(_), do: %{candidates: [], unresolved?: true,
                         interpreted_word_sense?: false, changes_lifg?: false}

  defp field(%{} = map, key),
    do: Map.get(map, key) || Map.get(map, Atom.to_string(key))
end
