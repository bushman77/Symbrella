defmodule Core.Response.TrustRepairFlowTest do
  use ExUnit.Case, async: true

  alias Core.Response

  test "trust rupture mood vetoes stale engineering fallback" do
    si = %{
      intent: :unknown,
      confidence: 0.4,
      text: "i think you are being a liar!!"
    }

    mood = %{:da => 0.48, "5ht" => 0.56, :glu => 0.54, :ne => 0.69}

    {tone, text, meta} = Response.plan(si, mood)

    assert tone == :deescalate
    assert meta.mode == :chat
    assert meta.action == :trust_repair
    assert meta.profile == :trust_repair
    assert :trust_repair in meta.overrides

    assert text =~ "You may be right to challenge me"
    assert text =~ "what felt dishonest"
    refute text =~ "next concrete target"
    refute text =~ "engineering move"
  end
end
