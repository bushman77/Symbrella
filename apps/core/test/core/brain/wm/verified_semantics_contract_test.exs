defmodule Core.Brain.WM.VerifiedSemanticsContractTest do
  use ExUnit.Case, async: false

  alias Core.Brain.WM.VerifiedSemantics

  setup do
    prior = Application.get_env(:core, VerifiedSemantics)
    Application.put_env(:core, VerifiedSemantics, enabled?: true)

    on_exit(fn ->
      if prior == nil do
        Application.delete_env(:core, VerifiedSemantics)
      else
        Application.put_env(:core, VerifiedSemantics, prior)
      end
    end)

    :ok
  end

  test "working-memory identities are deterministic and session-isolated" do
    session_a = "proof-session-a"
    session_b = "proof-session-b"

    assert VerifiedSemantics.frame_id(session_a) == VerifiedSemantics.frame_id(session_a)
    refute VerifiedSemantics.frame_id(session_a) == VerifiedSemantics.frame_id(session_b)
    assert String.starts_with?(VerifiedSemantics.frame_id(session_a), "verified_semantics:")
  end

  test "unverified, malformed and replayed request metadata cannot enter WM" do
    session = "unverified-#{System.unique_integer([:positive])}"
    names = ["Vancouver", "Burnaby"]

    assert :rejected ==
             VerifiedSemantics.admit(session, names, 2021, [], :verified_answer)

    assert :rejected ==
             VerifiedSemantics.admit(session, ["Vancouver", "Vancouver"], 2021, [1],
               :verified_answer)

    assert :rejected ==
             VerifiedSemantics.admit(session, names, 2021, [1, 1], :verified_answer)

    assert :rejected ==
             VerifiedSemantics.admit(session, names, 2021, [1], :untrusted_source)

    assert VerifiedSemantics.snapshot(session) == nil
  end

  test "rejection telemetry never reports factual authority" do
    listener = "verified-semantics-contract-#{System.unique_integer([:positive])}"
    parent = self()

    :ok =
      :telemetry.attach(
        listener,
        [:core, :wm, :verified_semantics],
        fn _, measurements, meta, pid ->
          send(pid, {:wm_rejection, measurements, meta})
        end,
        parent
      )

    try do
      assert :rejected == VerifiedSemantics.admit("test-session", ["A", "B"], 2021, [], :verified_answer)
      assert_receive {:wm_rejection, %{admitted: 0}, metadata}
      assert metadata.decision == :rejected
      assert metadata.factual_authority? == false
      assert metadata.admission_path == :core_revalidated_proofs
    after
      :telemetry.detach(listener)
    end
  end
end
