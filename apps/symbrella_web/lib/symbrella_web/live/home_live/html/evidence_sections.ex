defmodule SymbrellaWeb.HomeLive.HTML.EvidenceSections do
  @moduledoc """
  Explain modal sections rendered strictly from Core.SemanticInput-derived
  evidence. Labels retain runtime field names where precision matters.
  Neither dictionary verification nor device state is inferred from LLM text.
  """

  @spec build(map() | term()) :: [map()]
  def build(evidence) when is_map(evidence) do
    []
    |> runtime_section(field(evidence, :runtime))
    |> verified_section(field(evidence, :verified_phrasal_meanings))
    |> semantic_section(field(evidence, :semantic_winners))
    |> comprehension_section(field(evidence, :comprehension))
    |> grammar_section(
      field(evidence, :verb_particle_signatures),
      field(evidence, :atl_verb_particles)
    )
    |> hypotheses_section(field(evidence, :unverified_phrasal_hypotheses))
  end

  def build(_), do: []

  defp runtime_section(sections, runtime) when is_map(runtime) and map_size(runtime) > 0 do
    items =
      []
      |> value("intent", field(runtime, :intent))
      |> value("confidence", field(runtime, :confidence))
      |> value("conversation_act", field(runtime, :conversation_act))
      |> value("context_intent", field(runtime, :context_intent))
      |> value("selected_action", field(runtime, :selected_action))

    append(sections, :runtime_decision, "Core runtime decision", nil,
      "Values from the final SemanticInput, not inferred from the assistant's reply.",
      items
    )
  end

  defp runtime_section(sections, _), do: sections

  defp verified_section(sections, meanings) when is_list(meanings) and meanings != [] do
    items =
      Enum.map(meanings, fn m ->
        label = field(m, :id) || "Verified dictionary sense"

        body =
          []
          |> value("phrase", field(m, :phrase))
          |> value("definition", field(m, :definition))
          |> value("source", field(m, :source))
          |> value("context_score", field(m, :context_score))
          |> value("constituent_indices", field(m, :constituent_indices))
          |> value("object_index", field(m, :object_index))
          |> body()

        %{label: label, body: body}
      end)

    append(sections, :verified_phrasal_meanings, "Verified phrase meanings",
      Integer.to_string(length(items)),
      "Actual dictionary IDs; context_score is lexical evidence, NOT a probability or device-state measurement.",
      items
    )
  end

  defp verified_section(sections, _), do: sections

  defp semantic_section(sections, choices) when is_list(choices) and choices != [] do
    items =
      Enum.map(choices, fn c ->
        %{
          label: field(c, :id) || "Semantic choice",
          body:
            []
            |> value("token_index", field(c, :token_index))
            |> value("token_span", field(c, :token_span))
            |> value("kind", field(c, :kind))
            |> value("lemma", field(c, :lemma))
            |> value("source", field(c, :source))
            |> value("score", field(c, :score))
            |> value("definition", field(c, :definition))
            |> body()
        }
      end)

    append(sections, :semantic_choices, "Canonical semantic winners",
      Integer.to_string(length(items)),
      "Exact semantic_choices returned by Core. A contiguous MWE can replace its covered individual words here; discontinuous meanings appear above.",
      items
    )
  end

  defp semantic_section(sections, _), do: sections

  defp comprehension_section(sections, summary) when is_map(summary) do
    items =
      []
      |> value("intent", field(summary, :intent))
      |> value("understood", field(summary, :understood))
      |> value("uncertain", field(summary, :uncertain))
      |> value("unresolved", field(summary, :unresolved))
      |> value("degraded?", field(summary, :degraded?))
      |> value("reasons", field(summary, :reasons))

    append(sections, :comprehension, "Comprehension", nil,
      "Phrase-level meanings and raw weak-token diagnostics can coexist. Warnings are not suppressed.",
      items
    )
  end

  defp comprehension_section(sections, _), do: sections

  defp grammar_section(sections, signatures, constituents) do
    signatures = list(signatures)
    constituents = list(constituents)

    if signatures == [] and constituents == [] do
      sections
    else
      items =
        Enum.map(signatures, fn sig ->
          %{
            label: "Syntax #{show(field(sig, :span))}",
            body:
              []
              |> value("id", field(sig, :id))
              |> value("roles", field(sig, :roles))
              |> value("evidence", field(sig, :evidence))
              |> body()
          }
        end) ++
          Enum.map(constituents, fn con ->
            %{
              label: "ATL #{show(field(con, :span))}",
              body:
                []
                |> value("status", field(con, :status))
                |> value("roles", field(con, :roles))
                |> value("unresolved_roles", field(con, :unresolved_roles))
                |> body()
            }
          end)

      append(sections, :grammar_atl, "Syntax and ATL verb-particle roles", nil,
        "Syntax is a structural hypothesis; ATL records which individual roles actually resolved.",
        items
      )
    end
  end

  defp hypotheses_section(sections, hypotheses) when is_list(hypotheses) and hypotheses != [] do
    items =
      Enum.map(hypotheses, fn h ->
        %{
          label: field(h, :phrase) || "Possible phrasal verb",
          body:
            []
            |> value("span", field(h, :span))
            |> value("constituent_indices", field(h, :constituent_indices))
            |> value("particle_position", field(h, :particle_position))
            |> value("dictionary_check", field(h, :dictionary_check))
            |> value("verified?", field(h, :verified?))
            |> body()
        }
      end)

    append(sections, :unverified_phrasal_hypotheses, "Unverified syntax hypotheses", nil,
      "These were created before the separate dictionary verification step. A verified phrase above does not retroactively change these discovery records.",
      items
    )
  end

  defp hypotheses_section(sections, _), do: sections

  defp append(sections, key, title, tag, hint, items) do
    normalized_items =
      Enum.map(items, fn
        {label, datum} -> %{label: label, body: show(datum)}
        %{} = item -> item
      end)

    sections ++ [%{
      key: key, title: title, tag: tag, hint: hint, items: normalized_items
    }]
  end

  defp value(items, _label, nil), do: items
  defp value(items, label, datum), do: items ++ [{label, datum}]

  defp body(pairs) do
    Enum.map_join(pairs, "\n", fn {label, datum} -> "#{label}: #{show(datum)}" end)
  end

  defp show(nil), do: "nil"
  defp show(true), do: "true"
  defp show(false), do: "false"
  defp show(v) when is_atom(v), do: ":#{v}"
  defp show(v) when is_binary(v), do: v
  defp show(v) when is_number(v), do: to_string(v)
  defp show(v), do: inspect(v, limit: 40, printable_limit: 2_000)

  defp field(map, key) when is_map(map),
    do: Map.get(map, key, Map.get(map, Atom.to_string(key)))

  defp field(_, _), do: nil

  defp list(v) when is_list(v), do: v
  defp list(_), do: []
end
