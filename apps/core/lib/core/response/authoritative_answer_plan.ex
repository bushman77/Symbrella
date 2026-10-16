defmodule Core.Response.AuthoritativeAnswerPlan do
  @moduledoc """
  Authoritative response planning for conclusions already established by
  independently verified symbolic evidence.

  The LLM may select a *presentation* from a closed vocabulary. Core alone
  constructs the factual words, computations, dates and citations. Untrusted
  model prose is NEVER appended to the rendered factual answer.

  This first implementation is intentionally narrow: same-year two-place
  population comparisons with a valid source-supported comparison frame.
  """

  alias Core.Tools.WebRetrieval.Grounding

  @type style :: :brief | :detailed

  @spec build(map()) :: map() | nil
  def build(%{} = context) do
    evidence = Map.get(context, :web_evidence, %{})
    frame = Map.get(context, :response_frame, %{})
    support = if is_map(evidence), do: Map.get(evidence, :support), else: nil
    answers = Grounding.authorized_comparison_answers(context)

    with %{kind: :population_comparison, status: :source_supported,
           metric: :population, comparator: :greater_than,
           year: year, outcome: outcome, winner: winner,
           difference: difference, claims: claims} <- support,
         %{version: 1, topic: :population, comparison: :greater,
           evidence: %{kind: :population_comparison, year: ^year}} <- frame,
         %{brief: brief, detailed: detailed} <- answers,
         true <- is_binary(brief) and is_binary(detailed),
         true <- is_list(claims) and length(claims) == 2,
         true <- is_integer(year) and is_integer(difference),
         true <- Grounding.supports?(brief, context),
         true <- Grounding.supports?(detailed, context) do
      %{
        version: 1,
        kind: :verified_population_comparison,
        status: :source_supported,
        metric: :population,
        outcome: outcome,
        winner: winner,
        year: year,
        difference: difference,
        source_count: length(claims),
        preferred_style: :brief,
        allowed_answers: answers,
        provenance: :symbolic_verified_evidence
      }
    else
      _ -> nil
    end
  end

  def build(_), do: nil

  @doc "Only a closed style selector, not an open-ended answer, may come from the model."
  @spec style_selection_prompt(map()) :: list()
  def style_selection_prompt(%{kind: :verified_population_comparison}) do
    [
      %{
        "role" => "system",
        "content" =>
          "Choose the presentation style for an already verified factual answer. " <>
            "Output EXACTLY one word: BRIEF or DETAILED. " <>
            "BRIEF is concise and conversational; DETAILED includes both " <>
            "quantities on separate lines. Do not output facts, places, " <>
            "numbers, links, sentences, JSON, explanations, or punctuation."
      },
      %{
        "role" => "user",
        "content" => "Select BRIEF or DETAILED."
      }
    ]
  end

  def style_selection_prompt(_), do: []

  @spec select_style(term()) :: style()
  def select_style(value) when is_binary(value) do
    case value |> String.trim() |> String.downcase() do
      "detailed" -> :detailed
      _ -> :brief
    end
  end

  def select_style(_), do: :brief

  @spec render(map(), term()) :: String.t() | nil
  def render(%{kind: :verified_population_comparison, allowed_answers: answers}, style)
      when is_map(answers) do
    Map.get(answers, select_style_name(style)) ||
      Map.get(answers, :brief)
  end

  def render(_, _), do: nil

  @doc "This exact-match gate prevents post-plan language from changing any claim."
  @spec authorized?(map(), term(), map()) :: boolean()
  def authorized?(%{kind: :verified_population_comparison} = plan, answer, context)
      when is_binary(answer) and is_map(context) do
    answer in Map.values(Map.get(plan, :allowed_answers, %{})) and
      Grounding.supports?(answer, context)
  end

  def authorized?(_, _, _), do: false

  defp select_style_name(:detailed), do: :detailed
  defp select_style_name(_), do: :brief
end
