# Run with:
#   mix run scripts/stress_verb_particles_prod.exs
#   VERB_PARTICLE_STRESS=full mix run scripts/stress_verb_particles_prod.exs
#
# Runs the same *production* Core.resolve_input pipeline used by Phoenix,
# against the current lexical database. It may update normal cognitive
# runtime state, mood, and episodic memory. It does not control physical devices.
#
# Structural unit tests (no DB reads) live in:
#   apps/core/test/core/syntax/verb_particle_end_to_end_stress_test.exs

defmodule Symbrella.VerbParticleProductionSmoke do
  @options [
    mode: :prod,
    enrich_lexicon?: true,
    lexicon_stage?: true,
    lifg_opts: [grammar_weight: 1.0],
    session_id: "verb-particle-stress"
  ]

  @focus [
    {"Turn the lights off.", :primary},
    {"Turn off the lights.", :alternate}
  ]

  @extended [
    {"Turn it off.", :pronoun},
    {"Switch the fan off.", :synonym},
    {"Power the computer down.", :synonym},
    {"They turn the lights off.", :declarative},
    {"He lights off the rocket.", :lexical_verb},
    {"Turn the lights off?", :question},
    {"Lights off.", :ambiguous},
    {"Turn the lights quickly.", :nonparticle},
    {"Turn. the lights off.", :boundary}
  ]

  def run do
    full? = System.get_env("VERB_PARTICLE_STRESS") == "full"
    cases = if full?, do: @focus ++ @extended, else: @focus

    IO.puts("\n=== Symbrella verb-particle production smoke ===")
    IO.puts("Production cognition pipeline: normal mood/memory side effects are possible.")
    IO.puts("Mode: #{if(full?, do: "full", else: "focus")}\n")

    for {sentence, category} <- cases do
      IO.puts("\n--- #{inspect(sentence)} [#{category}] ---")

      try do
        if not db_ready?(), do: throw(:database_unavailable_before_case)
        si = Core.resolve_input(sentence, @options)
        if not db_ready?(), do: throw(:database_unavailable_after_case)
        report = summarize(si, category)

        IO.inspect(report, limit: :infinity, pretty: true, width: 100)

        if category in [:primary, :alternate] do
          checks = if(category == :primary, do: primary_checks(si), else: alternate_checks(si))
          IO.puts("\n#{String.upcase(to_string(category))} CHECKS:")
          Enum.each(checks, fn {name, ok?} ->
            IO.puts("#{if(ok?, do: "PASS", else: "FAIL")} #{name}")
          end)

          if Enum.any?(checks, fn {_name, ok?} -> not ok? end) do
            IO.puts("Result: production behavior still needs investigation.")
          else
            IO.puts("Result: #{category} syntax and lexical meaning contract satisfied.")
          end
        end
      rescue
        exception ->
          IO.puts("ERROR: #{Exception.format(:error, exception, __STACKTRACE__)}")
      catch
        kind, reason ->
          IO.puts("ERROR: #{inspect({kind, reason})}")
      end
    end

    IO.puts("\nReview lexical *sense definitions* separately from grammatical role resolution.")
  end

  defp db_ready? do
    match?({:ok, _}, Ecto.Adapters.SQL.query(Db, "SELECT 1", [], timeout: 3_000))
  rescue
    _ -> false
  catch
    _, _ -> false
  end

  defp summarize(si, category) do
    choices = list(Map.get(si, :lifg_choices))
    signatures = list(Map.get(si, :sentence_signatures))
    mwe = list(Map.get(si, :mwe_choices))
    frame = complete_particle_frame(signatures)
    atl = atlas_constituents(si)
    context = Map.get(si, :context_frame)

    %{
      category: category,
      intent: Map.get(si, :intent),
      confidence: Map.get(si, :confidence),
      conversation_act: Map.get(si, :conversation_act),
      context_intent: if(is_map(context), do: Map.get(context, :intent), else: nil),
      selected_action: Map.get(si, :selected_action),
      grammar_frame: frame,
      all_verb_particle_signatures:
        Enum.filter(signatures, &(Map.get(&1, :id) == :verb_particle_construction))
        |> Enum.map(&Map.take(&1, [:span, :roles, :evidence])),
      unverified_phrasal_hypotheses:
        Enum.filter(list(Map.get(si, :mwe_hypotheses)), fn h ->
          Map.get(h, :category) == :phrasal_verb
        end)
        |> Enum.map(&Map.take(&1, [:phrase, :span, :constituent_indices,
                                   :particle_position, :dictionary_check, :verified?])),
      semantic_winners:
        Enum.map(list(Map.get(si, :semantic_choices)), fn c ->
          Map.take(c, [:token_index, :id, :kind, :token_span, :lemma, :score, :definition])
        end),
      comprehension:
        case Map.get(si, :comprehension) do
          %{} = summary ->
            Map.take(summary, [:intent, :understood, :uncertain, :unresolved, :degraded?,
                               :reasons, :lexical_meanings])
          _ -> nil
        end,
      lexical_winners:
        Enum.map(choices, fn c ->
          %{
            index: Map.get(c, :token_index),
            id: Map.get(c, :id),
            pos: Map.get(c, :pos),
            gloss: Map.get(c, :definition)
          }
        end),
      mwe_winners:
        Enum.map(mwe, &Map.take(&1, [:id, :token_span, :source, :phrase, :definition])),
      verified_phrasal_meanings:
        Enum.map(list(Map.get(si, :lexical_constructions)), fn meaning ->
          Map.take(meaning, [:id, :phrase, :definition, :context_score,
                             :source, :object_index, :constituent_indices])
        end),
      atl_verb_particles:
        Enum.filter(atl, &(Map.get(&1, :id) == :verb_particle_construction))
        |> Enum.map(&Map.take(&1, [:span, :status, :roles, :unresolved_roles])),
      intent_refined?: Enum.any?(list(Map.get(si, :trace)), fn
        %{stage: :intent_refinement} -> true
        _ -> false
      end)
    }
  end

  defp primary_checks(si) do
    choices = list(Map.get(si, :lifg_choices))
    frame = complete_particle_frame(list(Map.get(si, :sentence_signatures)))
    atlas = atlas_constituents(si)
    selected = Map.new(choices, &{Map.get(&1, :token_index), Map.get(&1, :id)})

    [
      {"complete four-role verb-particle signature",
       is_map(frame) and Map.get(frame, :roles) == %{
         verb: 0, determiner: 1, object: 2, particle: 3
       }},
      {"all four word winners present", Enum.all?(0..3, &Map.has_key?(selected, &1))},
      {"lights resolved as noun", String.contains?(Map.get(selected, 2, ""), "|noun|")},
      {"off has an actual dictionary sense", is_binary(Map.get(selected, 3))},
      {"verified deactivation sense for separated phrasal verb",
       Enum.any?(list(Map.get(si, :lexical_constructions)), fn meaning ->
         meaning.id == "turn off|verb|2" and
           meaning.source == :verified_discontinuous_mwe
       end)},
      {"no spurious lights off verb MWE promoted",
       not Enum.any?(list(Map.get(si, :mwe_choices)), fn c ->
         Map.get(c, :token_span) == {2, 3} and
           String.starts_with?(Map.get(c, :id, ""), "lights off|verb|")
       end)},
      {"ATL complete four-role constituent",
       Enum.any?(atlas, fn c ->
         Map.get(c, :id) == :verb_particle_construction and
           Map.get(c, :span) == {0, 3} and
           Map.get(c, :status) == :resolved
       end)},
      {"command intent", Map.get(si, :intent) == :command},
      {"instruction conversation act", Map.get(si, :conversation_act) == :instruction},
      {"context intent consistent",
       is_map(Map.get(si, :context_frame)) and
         Map.get(Map.get(si, :context_frame) || %{}, :intent) == :command}
    ]
  end

  defp alternate_checks(si) do
    semantic = list(Map.get(si, :semantic_choices))
    meanings = list(Map.get(si, :lexical_constructions))

    [
      {"contiguous phrasal verb selects the deactivation sense",
       Enum.any?(semantic, &(&1.id == "turn off|verb|2"))},
      {"comprehension receives the verified dictionary meaning",
       Enum.any?(meanings, &(&1.id == "turn off|verb|2"))}
    ]
  end

  defp complete_particle_frame(signatures) do
    Enum.find(signatures, fn
      %{id: :verb_particle_construction,
        roles: %{verb: 0, determiner: 1, object: 2, particle: 3}} -> true
      _ -> false
    end)
    |> case do
      nil -> nil
      frame -> Map.take(frame, [:id, :span, :roles, :evidence])
    end
  end

  defp atlas_constituents(si) do
    case Map.get(si, :atl_slate) do
      %{} = slate -> list(Map.get(slate, :constituents))
      _ -> []
    end
  end

  defp list(v) when is_list(v), do: v
  defp list(_), do: []
end

Symbrella.VerbParticleProductionSmoke.run()
