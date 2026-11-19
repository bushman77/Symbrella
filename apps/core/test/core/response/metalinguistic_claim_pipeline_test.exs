defmodule Core.Response.MetalinguisticClaimPipelineTest do
  use ExUnit.Case, async: false

  alias Core.Discourse.Ledger
  alias Core.Discourse.Ledger.Owner
  alias Core.Brain.WM.Comprehension, as: WMComprehension

  defp session, do: "p0132-#{System.unique_integer([:positive])}"

  defp run(sentence, session, opts \\ []) do
    Core.resolve_input(sentence,
      Keyword.merge([
        mode: :prod,
        session_id: session,
        metalinguistic_claims?: true,
        response: :off,
        persist_episodes: false,
        execute_agency_commands?: false
      ], opts))
  end

  test "two live Core turns create a provisional claim with no response" do
    sid = session()
    on_exit(fn -> Ledger.clear(sid) end)
    first = run("zorple!", sid)
    marker = hd(Ledger.entries(sid))
    assert marker.kind == :uninterpreted
    assert marker.source_term == "zorple"
    assert byte_size(marker.source_turn_sha256) == 64
    assert first.metalinguistic_claim == nil

    second = run("it's a greeting.", sid)
    assert second.metalinguistic_claim.kind == :metalinguistic_claim
    assert second.metalinguistic_claim.source_seq == marker.seq
    assert second.metalinguistic_claim.epistemic.lexically_verified? == false
    assert second.response_text == nil
    [claim, ^marker] = Ledger.entries(sid)
    assert claim.kind == :metalinguistic_claim
    assert claim.class_term == "greeting"
    assert claim.source_seq == marker.seq
    assert claim.factual_authority? == false
    assert claim.epistemic.durably_persisted? == false
    assert {:error, :verified_metalinguistic_required} =
      Owner.append(sid, Map.delete(claim, :seq))
    assert {:error, :verified_metalinguistic_required} =
      Owner.append(sid, %{kind: :uninterpreted, source_term: "forged"})
  end

  test "the last turn, session boundary and five-minute window are mandatory" do
    sid = session()
    other = session()
    on_exit(fn -> Ledger.clear(sid); Ledger.clear(other) end)

    assert run("it's a greeting", sid).metalinguistic_claim == nil
    _ = run("florgix!", sid, metalinguistic_now_ms: 1_000)
    assert run("it's a greeting", sid,
      metalinguistic_now_ms: 301_001).metalinguistic_claim == nil

    _ = run("florgix!", other)
    assert run("it's a greeting", sid).metalinguistic_claim == nil

    _ = run("I have a dog.", other)
    assert run("it's a puppy", other).metalinguistic_claim == nil

    _ = run("florgix!", sid)
    _ = run("this is another turn", sid)
    assert run("it's a greeting", sid).metalinguistic_claim == nil
  end

  test "the claim does not become an antecedent for a contradictory follow-up" do
    sid = session()
    on_exit(fn -> Ledger.clear(sid) end)
    _ = run("florgix!", sid)
    assert run("it's a greeting", sid).metalinguistic_claim != nil
    assert run("it's a farewell", sid).metalinguistic_claim == nil
    assert Enum.count(Ledger.entries(sid), &(&1.kind == :metalinguistic_claim)) == 1
  end

  test "default-off flag retains no source lexeme or claim" do
    sid = session()
    on_exit(fn -> Ledger.clear(sid) end)
    first = run("zorple!", sid, metalinguistic_claims?: false)
    refute Map.get(first, :metalinguistic_claim)
    latest = hd(Ledger.entries(sid))
    refute Map.has_key?(latest, :source_term)
    second = run("it's a greeting", sid, metalinguistic_claims?: false)
    assert second.metalinguistic_claim == nil
    assert Enum.all?(Ledger.entries(sid),
      &(not Map.has_key?(&1, :source_term)))
  end

  test "the same two live turns may acknowledge without claiming verified truth" do
    prior = Application.get_env(:core, :llm_inference_enabled?, :unset)
    Application.put_env(:core, :llm_inference_enabled?, false)
    on_exit(fn ->
      if prior == :unset do
        Application.delete_env(:core, :llm_inference_enabled?)
      else
        Application.put_env(:core, :llm_inference_enabled?, prior)
      end
    end)

    sid = session()
    on_exit(fn ->
      Ledger.clear(sid)
      Brain.defocus(WMComprehension.frame_id(sid))
    end)
    _ = run("florgix!", sid)
    si = run("it's a greeting", sid,
      response: :auto,
      comprehension_wm?: true)

    assert is_map(si.wm_context)
    assert si.wm_context == WMComprehension.admitted_context(sid)
    assert si.metalinguistic_claim.kind == :metalinguistic_claim
    assert si.response_text =~ ~s|you're saying that "florgix" is a "greeting"|,
           "selected_action=#{inspect(si.selected_action)} " <>
             "intent=#{inspect(si.intent)} " <>
             "wm_transition=#{inspect(Map.get(si.wm_context || %{}, :focus_transition))} " <>
             "wm_admitted?=#{inspect(is_map(si.wm_context) and si.wm_context == WMComprehension.admitted_context(sid))} " <>
             "wm_attention=#{inspect(Map.get(si.wm_context || %{}, :attention))} " <>
             "response_source=#{inspect(Map.get(si.response_meta || %{}, :response_source))} " <>
             "planner_action=#{inspect(Map.get(si.response_meta || %{}, :action))}"
    assert si.response_text =~ "haven't independently verified"
    assert si.response_meta.response_reflection.status ==
      :metalinguistic_classification_acknowledged
    reflection = si.response_meta.response_reflection
    refute reflection.semantic_verified?
    refute reflection.factual_claims?
    refute reflection.dictionary_learned?
    refute reflection.llm_called?
    refute Map.has_key?(reflection, :source_term)
    refute Map.has_key?(reflection, :class_term)
  end
end
