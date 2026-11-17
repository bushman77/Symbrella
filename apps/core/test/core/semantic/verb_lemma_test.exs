defmodule Core.Semantic.VerbLemmaTest do
  use ExUnit.Case, async: true

  alias Core.Semantic.VerbLemma

  @choice %{
    token_index: 1,
    id: "bought|verb|1"
  }

  @cell %{
    id: "bought|verb|1",
    norm: "bought",
    pos: "verb",
    semantic_atoms: [
      "tag:form-of",
      "tag:past",
      "tag:participle"
    ],
    definition: "(form-of, participle, past) simple past and past participle of buy."
  }

  test "dual-tagged simple past resolves to its attested base verb" do
    si = %{active_cells: [@cell]}

    assert {:ok, "buy"} =
             VerbLemma.canonical_past(si, @choice)
  end

  test "past participle alone does not establish simple past" do
    cell = %{
      @cell
      | definition: "(form-of, participle, past) past participle of buy."
    }

    si = %{active_cells: [cell]}

    assert :error =
             VerbLemma.canonical_past(si, @choice)
  end

  test "canonicalization requires the exact selected dictionary cell" do
    si = %{active_cells: [%{@cell | id: "bought|verb|2"}]}

    assert :error =
             VerbLemma.canonical_past(si, @choice)
  end
end
