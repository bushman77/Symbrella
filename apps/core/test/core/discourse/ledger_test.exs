defmodule Core.Discourse.LedgerTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Discourse.Ledger.Owner
  alias Core.Discourse.References
  alias Core.Semantic.Composition
  alias Core.Syntax.{Signatures, StructuralCompetition}

  defp session do
    "discourse-ledger-test-#{System.unique_integer([:positive])}"
  end

  defp nominal(session, words) do
    positions = ~w(determiner adjective adjective noun)
    rows =
      Enum.zip(words, positions)
      |> Enum.map(fn {word, part} ->
        norm = String.downcase(word)
        %{id: "#{norm}|#{part}|1", norm: norm, pos: part, semantic_atoms: []}
      end)

    %Core.SemanticInput{
      session_id: session,
      source: :test,
      sentence: Enum.join(words, " ") <> ".",
      intent: :unknown,
      tokens:
        words |> Enum.with_index()
        |> Enum.map(fn {phrase, index} -> %{phrase: phrase, index: index, n: 1} end),
      active_cells: rows,
      lifg_choices:
        rows |> Enum.with_index()
        |> Enum.map(fn {row, index} -> %{id: row.id, token_index: index} end)
    }
    |> Signatures.attach()
    |> StructuralCompetition.attach()
    |> Composition.attach()
  end

  defp other_turn(session, index) do
    %Core.SemanticInput{
      session_id: session,
      source: :test,
      sentence: "Unrelated conversation turn #{index}.",
      intent: :unknown
    }
  end

  setup do
    assert is_pid(Process.whereis(Owner))
    sid = session()
    sid2 = session()
    on_exit(fn ->
      Ledger.clear(sid)
      Ledger.clear(sid2)
    end)
    {:ok, sid: sid, other: sid2}
  end

  test "verified nominal is recoverable after many unrelated turns", %{sid: sid} do
    original = nominal(sid, ~w(The small black bird))
    assert {:ok, proof} = Composition.verified(original)

    result = Ledger.record(original)
    assert result.sentence == original.sentence
    assert Enum.any?(result.trace, fn
      %{stage: :discourse_ledger, decision: :recorded,
        meta: %{sequence: 1, kind: :nominal_description}} -> true
      _ -> false
    end)

    for n <- 2..22 do
      Ledger.record(other_turn(sid, n))
    end

    assert length(Ledger.entries(sid)) == 22
    assert hd(Ledger.entries(sid)).seq == 22
    assert hd(Ledger.entries(sid)).kind == :uninterpreted
    assert List.last(Ledger.entries(sid)).seq == 1

    {:ok, record} = Ledger.resolve(sid, "Do you remember that black bird?")
    assert record.seq == 1
    assert record.head == "bird"
    assert record.modifiers == ["small", "black"]
    assert record.provenance == :revalidated_nominal_composition
    assert is_binary(record.proof_sha256)
    assert byte_size(record.proof_sha256) == 64
    assert record.factual_authority? == false
    refute Map.has_key?(record, :sentence)
    refute Map.has_key?(record, :real_world_existence)

    si =
      %Core.SemanticInput{session_id: sid, sentence: "Tell me about that black bird",
        source: :test}
      |> Ledger.recall()
      |> References.attach()

    assert si.discourse_context.status == :candidate_same_session
    assert si.discourse_reference.source == :discourse_ledger
    assert si.discourse_reference.antecedent == "small black bird"
    assert si.discourse_reference.antecedent_turn_seq == 1
    assert si.discourse_reference.factual_authority? == false

    # A uniquely explicit old mention re-enters WM; a subsequent pronoun can
    # use that active focus without rereading every historical sentence.
    focused = Core.Brain.WM.Comprehension.context_from(si, %{
      recognized_topics: [], understood: [], uncertain: [], degraded?: false
    })
    assert focused.nominal_focus.surface == "small black bird"
    assert focused.nominal_focus.reintroduced_from_ledger_seq == 1

    continued =
      %Core.SemanticInput{session_id: sid, source: :test,
        sentence: "It is sleeping under a tree.", wm_context: focused}
      |> References.attach()

    assert continued.discourse_reference.source == :wm_nominal_focus
    assert continued.discourse_reference.antecedent == "small black bird"
    assert :ok = Core.SemanticInput.validate_contract(si)
  end

  test "ambiguous noun references do not pick an arbitrary earlier bird", %{sid: sid} do
    Ledger.record(nominal(sid, ~w(The small black bird)))
    Ledger.record(other_turn(sid, 2))
    Ledger.record(nominal(sid, ~w(The white black bird)))

    assert {:ambiguous, candidates} = Ledger.resolve(sid, "What about that black bird?")
    assert length(candidates) == 2
    assert Enum.map(candidates, & &1.seq) == [3, 1]

    ambiguous =
      %Core.SemanticInput{session_id: sid, sentence: "What about that black bird?",
        source: :test}
      |> Ledger.recall()
      |> References.attach()

    assert ambiguous.discourse_context.status == :ambiguous
    assert length(ambiguous.discourse_context.candidates) == 2
    assert ambiguous.discourse_reference == nil
    denied = Core.Brain.WM.Comprehension.context_from(ambiguous, %{})
    assert denied.nominal_focus == nil

    # Full explicit mention disambiguates without guessing which "black bird".
    assert {:ok, %{surface: "small black bird", seq: 1}} =
      Ledger.resolve(sid, "I meant the small black bird.")
  end

  test "provisional spatial reports become attributed event records, never verified events",
       %{sid: sid} do
    first = nominal(sid, ~w(The small black bird))
    context = Core.Brain.WM.Comprehension.context_from(first, %{
      recognized_topics: [], understood: [], uncertain: [], degraded?: false
    })

    words = ~w(It is sitting on the fence)
    positions = ~w(pronoun verb verb preposition determiner noun)
    rows = Enum.zip(words, positions) |> Enum.map(fn {word, part} ->
      norm = String.downcase(word)
      %{id: "#{norm}|#{part}|1", norm: norm, pos: part,
        semantic_atoms: if(norm == "sitting",
          do: ["tag:form-of", "tag:present", "tag:participle"], else: [])}
    end)

    followup =
      %Core.SemanticInput{
        session_id: sid, source: :test, intent: :unknown,
        sentence: "It is sitting on the fence.",
        wm_context: context,
        tokens: words |> Enum.with_index()
          |> Enum.map(fn {word, idx} -> %{phrase: word, index: idx, n: 1} end),
        active_cells: rows,
        lifg_choices: rows |> Enum.with_index()
          |> Enum.map(fn {row, idx} -> %{id: row.id, token_index: idx} end)
      }
      |> References.attach()
      |> Signatures.attach()
      |> StructuralCompetition.attach()
      |> Core.Semantic.SentenceMeaning.attach()

    assert {:ok, _} = Core.Semantic.DiscourseComposition.verified(followup)
    Ledger.record(first)
    Ledger.record(followup)

    assert [%{kind: :reported_spatial_description, seq: 2, status: :provisional,
              antecedent: "small black bird", action: :sit, relation: :on,
              landmark: :fence, factual_authority?: false,
              event_occurrence_verified?: false, referent_verified?: false},
            %{kind: :nominal_description, seq: 1}] = Ledger.entries(sid)
  end

  test "no session crossing or long-distance bare-pronoun revival", %{sid: sid, other: other} do
    Ledger.record(nominal(sid, ~w(The small black bird)))

    assert :not_found = Ledger.resolve(other, "Remember that black bird?")
    assert :not_found = Ledger.resolve(sid, "What is it doing?")
    assert :not_found = Ledger.resolve(sid, "It is sleeping under a tree.")
    assert :not_found = Ledger.resolve(sid, "That is interesting.")
    assert Ledger.entries(other) == []

    wrong =
      %Core.SemanticInput{session_id: other,
        sentence: "Tell me about that black bird.", source: :test}
      |> Ledger.recall()
      |> References.attach()

    assert wrong.discourse_context == nil
    assert wrong.discourse_reference == nil

    unscoped =
      %Core.SemanticInput{sentence: "Tell me about that black bird.", source: :test}
      |> Ledger.recall()
    assert unscoped.discourse_context == nil
  end

  test "stale or forged semantic state records no fake nominal evidence", %{sid: sid} do
    original = nominal(sid, ~w(The small black bird))
    bad_proof = put_in(original, [Access.key!(:compositional_derivation),
      Access.key!(:result)], {:var, "fake"})

    assert Composition.verified(bad_proof) == :not_applicable
    Ledger.record(bad_proof)
    assert [%{kind: :uninterpreted, seq: 1}] = Ledger.entries(sid)
    assert :not_found = Ledger.resolve(sid, "that black bird")

    Ledger.record(%{original | source: :assistant})
    assert length(Ledger.entries(sid)) == 1

    Ledger.record(nominal(sid, ~w(The small black bird)))
    assert {:ok, %{seq: 2}} = Ledger.resolve(sid, "that black bird")
  end

  test "retention is explicitly bounded with monotonically increasing turn numbers",
       %{sid: sid} do
    old = Application.get_env(:core, Ledger, :unset)
    Application.put_env(:core, Ledger, max_entries_per_session: 3)

    on_exit(fn ->
      if old == :unset do
        Application.delete_env(:core, Ledger)
      else
        Application.put_env(:core, Ledger, old)
      end
    end)

    Ledger.record(nominal(sid, ~w(The small black bird)))

    for n <- 2..5 do
      Ledger.record(other_turn(sid, n))
    end

    assert Enum.map(Ledger.entries(sid), & &1.seq) == [5, 4, 3]
    assert :not_found = Ledger.resolve(sid, "that black bird")
    assert :ok = Ledger.clear(sid)
    assert Ledger.entries(sid) == []
  end

  test "disabled gate does not mutate ledger or leak a stale retrieval", %{sid: sid} do
    unchanged = Ledger.record(nominal(sid, ~w(The small black bird)),
      discourse_ledger?: false)
    assert unchanged.trace == []
    assert Ledger.entries(sid) == []

    Ledger.record(nominal(sid, ~w(The small black bird)))
    input = %Core.SemanticInput{session_id: sid,
      source: :test, sentence: "that black bird"}

    assert Ledger.recall(input, discourse_ledger?: false) == input
    assert Ledger.resolve(sid, "that black bird") != :not_found
  end
end
