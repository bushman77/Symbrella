defmodule Core do
  @moduledoc """
  Central Core pipeline (skeleton).

  `resolve_input/2` is the ONLY entrypoint from Brain. We’re carrying forward
  the legacy pipeline structure (segment → tokenize → activate → POS/intent → plan),
  but all integrations are commented out until we wire them in carefully.
  """

  require Logger

  # ───────────── Legacy aliases (commented out until wired) ─────────────
  # alias Axon
  # alias Brain
  # alias BrainCell
  alias Core.{Token}
#, IntentClassifier, IntentResolver, IntentPOSProfile,
  #             POSDisambiguator, POSEngine, ResponsePlanner, SemanticInput, Token, DB}
  # alias FRP.Features
  # alias MoodCore
  # alias Core.MultiwordPOS

  @type input_t :: %{
          text: binary(),
          tokens: list()  # later: [%Core.Token{}]
        }

  @type reply_t :: %{
          text: binary(),
          intent: atom(),
          confidence: float(),
          placeholder: boolean()
        }

  @doc """
  Master pipeline (skeleton).

  For now this returns an echo placeholder, but preserves the legacy
  step-by-step layout in comments so we can re-enable each stage safely.
  """
  @spec resolve_input(String.t(), keyword()) :: {input_t, reply_t}
  def resolve_input(input, _opts \\ []) when is_binary(input) do
    # ───────────────────────── 1) Greedy segmentation ────────────────
    # segs = segment_phrases(input)

    # ───────────────────────── 2) Tokenize (from segments if present) 
    sem =
    #   try do
    #     Tokenizer.from_segments(segs, source)
    #   rescue
    #     _ ->
      input
      |> Token.tokenize()
    #       |> SemanticInput.sanitize()
    #       |> Map.put(:source, source)
    #   end
    IO.inspect sem

    # ───────────────────────── 3) PRE-INTENT: ensure rows & start processes ─────────────────────────
    # pos_by_phrase =
    #   sem.token_structs
    #   |> Enum.reduce(%{}, fn t, acc -> Map.put_new(acc, normalize(t.phrase), Map.get(t, :pos)) end)
    #
    # activation_candidates =
    #   (segs |> Enum.map(& &1.text)) ++ (sem.token_structs |> Enum.map(& &1.phrase))
    #   |> Enum.map(&normalize/1)
    #   |> Enum.uniq()
    #   |> Enum.filter(&allow_activation?/1)
    #
    # Logger.info("Activating: #{inspect(activation_candidates)}")
    #
    # Enum.each(activation_candidates, fn ph ->
    #   ret =
    #     if function_exported?(Brain, :get_or_start, 2) do
    #       Brain.get_or_start(ph, Map.get(pos_by_phrase, ph))
    #     else
    #       Brain.get_or_start(ph)
    #     end
    #   # … resolve pid variants & register active …
    # end)
    #
    # attention_tokens =
    #   activation_candidates
    #   |> Enum.map(fn ph -> %Token{text: ph, phrase: ph, pos: Map.get(pos_by_phrase, ph)} end)
    # Brain.attention(attention_tokens)

    # ───────────────────────── 4) Downstream POS + Intent + Features + Mood + Plan ─────────────────────────
    # sem
    # |> POSEngine.tag()
    # |> then(fn sem2 ->
    #   chosen = POSDisambiguator.disambiguate(sem2.token_structs)
    #   Map.put(sem2, :chosen_cells, chosen)
    # end)
    # |> IntentClassifier.classify_tokens()
    # |> IntentResolver.resolve_intent()
    # |> Features.attach_features()
    # |> IntentResolver.refine_with_pos_profiles()
    # |> Brain.prune_by_intent_pos()
    # |> MoodCore.attach_mood()
    # |> ResponsePlanner.analyze()
    # |> then(&{:ok, &1})

    # ───────────────────────── Temporary: safe echo output ─────────────────────────
    norm =
      input
      |> String.trim_trailing()
      |> String.replace(~r/(?:\r\n|\r|\n){3,}/, "\n\n")

    input_view = %{
      text: norm,
      tokens: []  # will be [%Core.Token{}] once Tokenizer is wired
    }

    reply = %{
      text: "You said this: " <> norm,
      intent: :echo,
      confidence: 1.0,
      placeholder: true
    }

    {input_view, reply}
  end

  # ───────────── Legacy helpers (kept here but commented out until wired) ─────────────
  # defp segment_phrases(sentence) do
  #   words =
  #     Regex.scan(~r/\p{L}+|\d+|[^\s\p{L}\d]+/u, sentence)
  #     |> Enum.map(&hd/1)
  #   max_n = 5
  #   do_segment(words, 0, max_n, []) |> Enum.reverse()
  # end
  #
  # defp do_segment(words, i, max_n, acc) when i >= length(words), do: acc
  # defp do_segment(words, i, max_n, acc) do
  #   end_idx = min(length(words), i + max_n)
  #   {hit_phrase, span} =
  #     i..(end_idx - 1)
  #     |> Enum.reverse()
  #     |> Enum.find_value({nil, 0}, fn j ->
  #       phrase =
  #         words
  #         |> Enum.slice(i..j)
  #         |> Enum.join(" ")
  #         |> normalize()
  #       if phrase_known_locally?(phrase), do: {phrase, j - i + 1}, else: nil
  #     end) || {nil, 0}
  #   if hit_phrase do
  #     do_segment(words, i + span, max_n, [%{type: :phrase, text: hit_phrase} | acc])
  #   else
  #     w = normalize(Enum.at(words, i))
  #     do_segment(words, i + 1, max_n, [%{type: :word, text: w} | acc])
  #   end
  # end
  #
  # defp normalize(s), do: s |> String.downcase() |> String.trim()
  #
  # defp phrase_known_locally?(_phrase), do: false
  # defp allow_activation?(phrase), do: String.length(phrase) >= 2 and not String.contains?(phrase, " ")
end

