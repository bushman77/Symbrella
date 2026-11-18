defmodule Core.Syntax.NamedDirectionalTest do
  use ExUnit.Case, async: true

  alias Core.Pipeline.Input
  alias Core.Syntax.Signatures

  defp input(sentence, name, verb, base) do
    si = Input.prepare(sentence)

    cells = [
      %{
        id: "#{String.downcase(name)}|proper_noun|1",
        norm: String.downcase(name),
        pos: "proper_noun"
      },
      %{
        id: "#{verb}|verb|1",
        norm: verb,
        pos: "verb",
        semantic_atoms: ["tag:form-of", "tag:past"],
        definition: "simple past of #{base}"
      },
      %{id: "to|preposition|1", norm: "to", pos: "preposition"},
      %{id: "the|determiner|1", norm: "the", pos: "determiner"},
      %{id: "store|noun|1", norm: "store", pos: "noun"},
      %{id: "park|noun|1", norm: "park", pos: "noun"}
    ]

    %{si | source: :test, active_cells: cells}
  end

  defp directional(si) do
    Signatures.detect(si)
    |> Enum.filter(&(&1.id == :finite_past_named_directional_candidate))
  end

  test "recognizes an attested named-subject directional clause" do
    si =
      input(
        "John went to the store.",
        "John",
        "went",
        "go"
      )

    assert [
             %{
               span: {0, 4},
               roles: %{
                 subject_head: 0,
                 predicate: 1,
                 preposition: 2,
                 destination_determiner: 3,
                 destination_head: 4
               },
               source: :core_syntax
             }
           ] = directional(si)
  end

  test "generalizes to other names, verbs and destinations" do
    si =
      input(
        "Alex walked to the park.",
        "Alex",
        "walked",
        "walk"
      )

    assert [_] = directional(si)
  end

  test "capitalization alone cannot establish a named subject" do
    si =
      input(
        "John went to the store.",
        "John",
        "went",
        "go"
      )

    cells =
      Enum.reject(
        si.active_cells,
        &(&1.pos == "proper_noun")
      )

    assert directional(%{si | active_cells: cells}) == []
  end

  test "past morphology must be independently attested" do
    si =
      input(
        "John went to the store.",
        "John",
        "went",
        "go"
      )

    cells =
      Enum.map(si.active_cells, fn
        %{pos: "verb"} = cell ->
          %{cell | semantic_atoms: []}

        cell ->
          cell
      end)

    assert directional(%{si | active_cells: cells}) == []
  end

  test "static location is not mistaken for directed motion" do
    si =
      input(
        "John went under the store.",
        "John",
        "went",
        "go"
      )

    assert directional(si) == []
  end
end
