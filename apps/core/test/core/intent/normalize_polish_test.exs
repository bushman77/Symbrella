defmodule Core.Intent.NormalizePolishTest do
  use ExUnit.Case, async: true
  alias Core.Intent.Normalize

  describe "intent normalization" do
    test "trims whitespace and strips edge punctuation" do
      assert %{intent: :ask} = Normalize.normalize(%{"intent" => "  Ask?  "})
      assert %{intent: :ask} = Normalize.normalize(%{"intent" => "Ask!!"})
      assert %{intent: :greet} = Normalize.normalize(%{"intent" => "  hello,  "})
    end

    test "alias map routes common phrases without creating atoms" do
      assert %{intent: :greet} = Normalize.normalize(%{"intent" => "HEY"})
      assert %{intent: :affirm} = Normalize.normalize(%{"intent" => "yes"})
      assert %{intent: :deny} = Normalize.normalize(%{"intent" => "n"})
    end

    test "unknown strings do not create atoms" do
      bogus = "foozle__never_make_me_an_atom"
      # guard rail: ensure the atom does not exist
      assert_raise ArgumentError, fn -> String.to_existing_atom(bogus) end

      assert %{intent: :unknown} = Normalize.normalize(%{"intent" => bogus})

      # still should not exist after normalization
      assert_raise ArgumentError, fn -> String.to_existing_atom(bogus) end
    end
  end

  describe "keyword normalization" do
    test "accepts atoms and strings and trims" do
      assert %{keyword: "hello"} = Normalize.normalize(%{keyword: :hello})
      assert %{keyword: "hello"} = Normalize.normalize(%{"keyword" => "  hello  "})
    end

    test "non-string/atom keyword becomes nil" do
      assert %{keyword: nil} = Normalize.normalize(%{"keyword" => 123})
    end
  end

  describe "confidence normalization" do
    test "accepts numbers and clamps to [0,1]" do
      assert %{confidence: 1.0} = Normalize.normalize(%{"confidence" => 2.7})
      assert %{confidence: 0.0} = Normalize.normalize(%{"confidence" => -1})
      assert %{confidence: 0.42} = Normalize.normalize(%{"confidence" => 0.42})
    end

    test "parses floats from strings and clamps" do
      assert %{confidence: 1.0} = Normalize.normalize(%{"confidence" => " 1.2 "})
      assert %{confidence: 0.25} = Normalize.normalize(%{"confidence" => "0.25"})
    end

    test "supports percent-form strings" do
      assert %{confidence: 0.7} = Normalize.normalize(%{"confidence" => "70%"})
      assert %{confidence: 1.0} = Normalize.normalize(%{"confidence" => "250%"})
    end

    test "junk confidence becomes nil" do
      assert %{confidence: nil} = Normalize.normalize(%{"confidence" => "NaN"})
      assert %{confidence: nil} = Normalize.normalize(%{"confidence" => %{}}) 
    end
  end

  describe "non-map inputs" do
    test "returns safe defaults" do
      assert %{intent: :unknown, keyword: nil, confidence: nil} = Normalize.normalize(123)
      assert %{intent: :unknown, keyword: nil, confidence: nil} = Normalize.normalize(nil)
      assert %{intent: :unknown, keyword: nil, confidence: nil} = Normalize.normalize("ask")
    end
  end
end

