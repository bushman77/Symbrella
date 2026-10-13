# Run from umbrella root with Phoenix Endpoint disabled when another server
# owns port 4000 (same --no-start wrapper as stress_verb_particles_prod.exs).
#
#   EXPLAIN_PARITY=full mix run --no-start -e '
#     endpoint = SymbrellaWeb.Endpoint
#     conf = Application.fetch_env!(:symbrella_web, endpoint)
#     Application.put_env(:symbrella_web, endpoint, Keyword.put(conf, :server, false))
#     {:ok, _} = Application.ensure_all_started(:symbrella_web)
#     Code.require_file("scripts/stress_html_explain_parity_prod.exs")
#   '
#
# No network request or live-LLM output is needed: same Core.resolve_input/2
# as LiveView and the exact Modal.explain_payload_for/1 HTML data path.
defmodule Symbrella.StressHTMLExplainParity do
  alias SymbrellaWeb.HomeLive.ExplainEvidence
  alias SymbrellaWeb.HomeLive.HTML.Modal

  @core_opts [
    mode: :prod,
    enrich_lexicon?: true,
    lexicon_stage?: true,
    lifg_opts: [grammar_weight: 1.0],
    session_id: "html-explain-parity"
  ]

  @focus [
    {"Turn the lights off.", "turn off|verb|2"},
    {"Turn off the lights.", "turn off|verb|2"},
    {"Power the computer down.", "power down|verb|1"}
  ]

  @extra [
    {"Switch the fan off.", "switch off|verb|1"},
    {"Turn it off.", nil},
    {"Turn. the lights off.", nil}
  ]

  def run do
    full = System.get_env("EXPLAIN_PARITY") == "full"
    cases = if(full, do: @focus ++ @extra, else: @focus)

    IO.puts("\n=== Core vs HTML Explain production parity ===")
    IO.puts("Cases: #{length(cases)}, mode=#{if(full, do: "full", else: "focus")}")
    IO.puts("Does not execute device controls; cognition may have normal memory/mood side effects.")

    outcomes =
      Enum.map(cases, fn {sentence, expected_id} ->
        IO.puts("\n--- #{inspect(sentence)} ---")
        run_case(sentence, expected_id)
      end)

    IO.puts("\nSUMMARY: PASS=#{Enum.count(outcomes, &(&1 == :pass))} " <>
      "FAIL=#{Enum.count(outcomes, &(&1 == :fail))} " <>
      "INCOMPLETE=#{Enum.count(outcomes, &(&1 == :incomplete))}")
  end

  defp run_case(sentence, expected_id) do
    start0 = db_start()

    if is_nil(start0) do
      IO.puts("INCOMPLETE PostgreSQL unavailable before case")
      :incomplete
    else
      try do
        si = Core.resolve_input(sentence, @core_opts)
        start1 = db_start()

        if is_nil(start1) or start1 != start0 do
          IO.puts("INCOMPLETE PostgreSQL stopped/restarted during case")
          :incomplete
        else
          validate(si, expected_id)
        end
      rescue
        e ->
          IO.puts("FAIL #{Exception.format(:error, e, __STACKTRACE__)}")
          :fail
      catch
        kind, reason ->
          IO.puts("INCOMPLETE #{inspect({kind, reason})}")
          :incomplete
      end
    end
  end

  defp validate(si, expected_id) do
    evidence = ExplainEvidence.from_si(si)

    payload =
      Modal.explain_payload_for(%{
        text: Map.get(si, :response_text) || "[no response generated]",
        intent: Map.get(si, :intent),
        confidence: Map.get(si, :confidence),
        cognition: evidence,
        from: Map.get(si, :response_meta) || %{},
        selected_action: Map.get(si, :selected_action),
        action_meta: Map.get(si, :action_meta)
      })

    canonical_ids = Enum.map(list(Map.get(si, :semantic_choices)), &Map.get(&1, :id))
    html_canonical_ids = labels(payload, :semantic_choices)
    selected_phrase_ids =
      si
      |> Map.get(:lexical_constructions)
      |> list()
      |> Enum.filter(&(Map.get(&1, :dictionary_verified?) == true))
      |> Enum.map(&Map.get(&1, :id))

    html_phrases = labels(payload, :verified_phrasal_meanings)

    checks = [
      {"all canonical SemanticInput IDs match HTML exactly",
       canonical_ids == html_canonical_ids},
      {"all verified dictionary construction IDs match HTML exactly",
       selected_phrase_ids == html_phrases},
      {"dictionary source, definition, and context scores match HTML",
       match_phrase_evidence(si, payload)},
      {"canonical dictionary gloss and scoring match HTML",
       match_semantic_details(si, payload)},
      {"syntax and ATL statuses match HTML",
       match_grammar_details(si, payload)},
      {"expected phrase-level sense matches",
       if(is_binary(expected_id),
         do: expected_id in selected_phrase_ids,
         else: selected_phrase_ids == [])},
      {"original Core comprehension degradation flag retained",
       check_comprehension(si, payload)},
      {"Core action selector evidence retained",
       check_action(si, payload)},
      {"LLM reflection status and repair are faithful to Core response metadata",
       check_response_reflection(si, payload)}
    ]

    Enum.each(checks, fn {label, passed?} ->
      IO.puts("#{if(passed?, do: "PASS", else: "FAIL")} #{label}")
    end)

    IO.inspect(%{
      intent: Map.get(si, :intent),
      verified_phrasal_meanings: html_phrases,
      semantic_ids: html_canonical_ids,
      comprehension: Map.get(si, :comprehension) |> summary(),
      source: :final_semantic_input
    }, pretty: true)

    if Enum.all?(checks, fn {_label, ok?} -> ok? end), do: :pass, else: :fail
  end

  defp match_phrase_evidence(si, payload) do
    meanings =
      si
      |> Map.get(:lexical_constructions)
      |> list()
      |> Enum.filter(&(Map.get(&1, :dictionary_verified?) == true))

    items = section_items(payload, :verified_phrasal_meanings)

    length(meanings) == length(items) and
      Enum.all?(Enum.zip(meanings, items), fn {meaning, item} ->
        item.label == Map.get(meaning, :id) and
          contains?(item.body, "definition: ", Map.get(meaning, :definition)) and
          contains?(item.body, "source: :", Map.get(meaning, :source)) and
          contains?(item.body, "context_score: ", Map.get(meaning, :context_score)) and
          contains?(item.body, "object_index: ", Map.get(meaning, :object_index))
      end)
  end

  defp match_semantic_details(si, payload) do
    choices = list(Map.get(si, :semantic_choices))
    items = section_items(payload, :semantic_choices)

    length(choices) == length(items) and
      Enum.all?(Enum.zip(choices, items), fn {choice, item} ->
        item.label == Map.get(choice, :id) and
          contains?(item.body, "definition: ", Map.get(choice, :definition)) and
          contains?(item.body, "score: ", Map.get(choice, :score))
      end)
  end

  defp match_grammar_details(si, payload) do
    sigs =
      si
      |> Map.get(:sentence_signatures)
      |> list()
      |> Enum.filter(&(Map.get(&1, :id) == :verb_particle_construction))

    atlas =
      si
      |> Map.get(:atl_slate, %{})
      |> case do
        %{} = state -> Map.get(state, :constituents)
        _ -> []
      end
      |> list()
      |> Enum.filter(&(Map.get(&1, :id) == :verb_particle_construction))

    items = section_items(payload, :grammar_atl)
    signatures_match? =
      Enum.all?(sigs, fn sig ->
        Enum.any?(items, fn item ->
          item.label == "Syntax #{inspect(Map.get(sig, :span))}" and
            contains?(item.body, "id: :", Map.get(sig, :id))
        end)
      end)

    atlas_match? =
      Enum.all?(atlas, fn con ->
        Enum.any?(items, fn item ->
          item.label == "ATL #{inspect(Map.get(con, :span))}" and
            contains?(item.body, "status: :", Map.get(con, :status))
        end)
      end)

    signatures_match? and atlas_match?
  end

  defp contains?(_body, _prefix, nil), do: true
  defp contains?(body, prefix, value) when is_binary(body) do
    String.contains?(body, prefix <> to_string(value))
  end

  defp section_items(payload, key) do
    case Enum.find(Map.get(payload, :sections, []), &(Map.get(&1, :key) == key)) do
      %{items: items} -> items
      _ -> []
    end
  end

  defp labels(payload, key) do
    case Enum.find(Map.get(payload, :sections, []), &(Map.get(&1, :key) == key)) do
      %{items: items} -> Enum.map(items, &Map.get(&1, :label))
      _ -> []
    end
  end

  defp check_response_reflection(si, payload) do
    meta = Map.get(si, :response_meta) || %{}
    reflection = Map.get(meta, :response_reflection)
    section = Enum.find(Map.get(payload, :sections, []),
      &(&1.key == :response_reflection))

    case reflection do
      %{status: status, issues: issues, applied?: applied?} ->
        is_map(section) and
          Enum.any?(section.items, fn i ->
            i.label == "status" and i.body == ":#{status}"
          end) and
          Enum.any?(section.items, fn i ->
            i.label == "applied?" and i.body == to_string(applied?)
          end) and
          Enum.any?(section.items, fn i ->
            i.label == "issues" and
              Enum.all?(issues, &String.contains?(i.body, Atom.to_string(&1)))
          end)

      _ ->
        is_nil(section)
    end
  end

  defp check_comprehension(si, payload) do
    c = Map.get(si, :comprehension)

    if is_map(c) do
      target = Enum.find(Map.get(payload, :sections, []), &(&1.key == :comprehension))

      is_map(target) and
        Enum.any?(target.items, fn item ->
          item.label == "degraded?" and
            item.body == to_string(Map.get(c, :degraded?))
        end)
    else
      not Enum.any?(Map.get(payload, :sections, []), &(&1.key == :comprehension))
    end
  end

  defp check_action(si, payload) do
    action = Map.get(si, :selected_action)
    if is_atom(action) do
      section = Enum.find(Map.get(payload, :sections, []),
        &(&1.key == :runtime_decision))
      is_map(section) and Enum.any?(section.items,
        &(&1.label == "selected_action" and &1.body == ":#{action}"))
    else
      true
    end
  end

  defp db_start do
    case Ecto.Adapters.SQL.query(Db, "SELECT pg_postmaster_start_time()", [], timeout: 3_000) do
      {:ok, %{rows: [[value]]}} -> value
      _ -> nil
    end
  rescue
    _ -> nil
  catch
    _, _ -> nil
  end

  defp summary(%{} = value),
    do: Map.take(value, [:degraded?, :reasons, :understood, :uncertain])
  defp summary(_), do: nil

  defp list(value) when is_list(value), do: value
  defp list(_), do: []
end

Symbrella.StressHTMLExplainParity.run()
