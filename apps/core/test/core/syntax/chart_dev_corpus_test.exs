defmodule Core.Syntax.ChartDevCorpusTest do
  use ExUnit.Case, async: true

  alias Core.SemanticInput
  alias Core.Syntax.{Chart, Signatures}
  alias Core.TokenFilters

  # Development-only, human-visible partition. The sealed evaluation corpus
  # is deliberately not referenced or reconstructed anywhere in this test.
  @corpus Path.expand("../../../../../docs/collab/p021/dev_corpus.md", __DIR__)

  @rows (
    @corpus
    |> File.read!()
    |> String.split("\n")
    |> Enum.filter(&String.match?(&1, ~r/^\|\s*D\d\d\s*\|/))
    |> Enum.map(fn line ->
      case line |> String.split("|", trim: true) |> Enum.map(&String.trim/1) do
        [id, text, category, grammar, semantic, phase, rationale] ->
          %{id: id, text: text, category: category, grammar: grammar,
            semantic: semantic, phase: phase, rationale: rationale}
        other -> raise "invalid visible DEV row: #{inspect(other)}"
      end
    end)
  )

  # Fixed, *synthetic* dictionary witnesses for grammar testing only. These
  # are not claims about live LTM coverage. Missing real LTM POS must cause a
  # lexical-witness abstention; grammar must never synthesize a dictionary row.
  @lexical [
    {"the", "determiner", []}, {"this", "determiner", []},
    {"a", "determiner", []}, {"site", "noun", []},
    {"page", "noun", []}, {"pages", "noun", ["tag:form-of", "tag:plural"]},
    {"market", "noun", []}, {"files", "noun", ["tag:form-of", "tag:plural"]},
    {"report", "noun", []}, {"wallet", "noun", []},
    {"agent", "noun", []}, {"agents", "noun", ["tag:form-of", "tag:plural"]},
    {"reviewer", "noun", []}, {"readers", "noun", ["tag:form-of", "tag:plural"]},
    {"small", "adjective", []}, {"open", "adjective", []},
    {"ready", "adjective", []}, {"public", "adjective", []},
    {"written", "adjective", []},
    {"written", "verb", ["tag:form-of", "tag:past", "tag:participle"]},
    {"stored", "adjective", []},
    {"stored", "verb", ["tag:form-of", "tag:past", "tag:participle"]},
    {"is", "auxiliary", []}, {"are", "auxiliary", []},
    {"was", "auxiliary", []}, {"were", "auxiliary", []},
    {"for", "preposition", []}, {"in", "preposition", []},
    {"by", "preposition", []}, {"too", "adverb", []},
    {"clearly", "adverb", []},
    {"you", "pronoun", []}, {"it", "pronoun", []},
    {"ignore", "verb", []}, {"run", "verb", []},
    {"publish", "verb", []}, {"point", "verb", []},
    {"send", "verb", []}, {"share", "verb", []}
  ]

  @abstentions %{
    "negation" => :negation,
    "modal" => :modal,
    "interrogative" => :interrogative,
    "agreement_conflict" => :agreement_conflict,
    "lexical_witness_missing" => :lexical_witness_missing,
    "ambiguous_attachment" => :ambiguous_attachment,
    "coverage_gap" => :coverage_gap,
    "no_clause_core" => :no_clause_core,
    "multiple_clauses" => :multiple_clauses,
    "instruction_shaped" => :instruction_shaped,
    "structured_record" => :structured_record
  }

  defp input(text) do
    cells =
      for {word, pos, tags} <- @lexical do
        %{id: "#{word}|#{pos}|1", norm: word, pos: pos, semantic_atoms: tags}
      end

    %SemanticInput{source: :colony_external, sentence: text, active_cells: cells}
    |> TokenFilters.rebuild_word_ngrams(3)
  end

  test "the complete visible DEV partition is loaded without touching sealed data" do
    assert length(@rows) == 36
    assert Enum.map(@rows, & &1.id) ==
      for(n <- 1..36, do: "D" <> String.pad_leading(Integer.to_string(n), 2, "0"))

    assert Enum.count(@rows, &String.starts_with?(&1.semantic, "SUP")) == 14
    assert Enum.count(@rows, &(&1.phase == "later")) == 1
    assert Enum.count(@rows, &String.starts_with?(&1.semantic, "ABSTAIN")) == 22
  end

  for row <- @rows do
    @tag dev_corpus: true
    test "#{row.id}: #{row.grammar} — P-021a L1 only" do
      row = unquote(Macro.escape(row))
      si = input(row.text)
      nodes = Signatures.chart_nodes(si)
      report = Chart.diagnose(si, nodes,
        Signatures.detect(si) |> Enum.filter(&(&1.id == :possessive_noun_phrase)))

      signatures = Signatures.detect(si)
        |> Enum.filter(&(&1.id == :copular_predication))

      assert signatures == report.signatures
      assert Enum.all?(signatures, &(Map.get(&1, :factual_authority?) == false))
      assert Enum.all?(signatures, &(not Map.has_key?(&1.roles, :agent)))
      assert Enum.all?(signatures, &(not Map.has_key?(&1, :action)))
      assert Enum.all?(signatures, &(not Map.has_key?(&1, :tool_call)))

      if String.starts_with?(row.semantic, "SUP") do
        assert report.status == :candidate,
          "#{row.id} expected one L1 hypothesis, got #{inspect(report.reason)}"
        assert length(signatures) == 1,
          "#{row.id} expected one L1 hypothesis, got #{length(signatures)}"
        assert hd(signatures).span == {0, length(nodes) - 1}
      else
        [_, reason] = Regex.run(~r/ABSTAIN\(([^)]+)\)/, row.semantic)
        expected = Map.fetch!(@abstentions, reason)
        assert signatures == [],
          "#{row.id}: an unsupported full-span candidate was emitted"
        assert report.reason == expected,
          "#{row.id}: expected #{expected}, observed #{inspect(report.reason)}"
      end
    end
  end
end
