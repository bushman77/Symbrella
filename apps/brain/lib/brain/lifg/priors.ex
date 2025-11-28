defmodule Brain.LIFG.Priors do
  @moduledoc """
  Relational priors for LIFG sense scoring (`rel_prior` feature).

  Pure & stateless:
  - parses candidate IDs like `"hey|verb|0"` or `"hey how is|phrase|fallback"`.
  - assigns a small prior based on POS + tag.
  - gives phrases (esp. phrase fallbacks) a gentle edge in flat slates.

  This never calls Core modules or touches DB — it only looks at the ID string.
  """

  @type id :: String.t()

  @type ctx :: %{
          optional(:intent) => atom(),
          optional(:token_index) => non_neg_integer(),
          optional(:si_sentence) => String.t()
        }

  # Base priors for POS — tiny differences, just enough to break ties.
  #
  # These are deliberately close to each other; the feature mix weights (e.g. 0.35
  # for :rel_prior) will turn these into small nudges, not hard rules.
  @pos_prior %{
    "phrase" => 1.00,
    "verb" => 0.97,
    "adj" => 0.96,
    "adv" => 0.95,
    "noun" => 0.94,
    "fallback" => 0.92
  }

  # Very conservative bounds; keeps rel_prior nicely inside [0.5, 1.2].
  @min_prior 0.5
  @max_prior 1.2

  @doc """
  Compute a `rel_prior` scalar for a given candidate ID.

  You can pass an optional context map (intent, token_index, si_sentence)
  if you want to evolve heuristics later; for now it's unused except for
  future-proofing.
  """
  @spec rel_prior(id(), ctx()) :: float()
  def rel_prior(id, ctx \\ %{}) when is_binary(id) and is_map(ctx) do
    {lemma, pos, tag} = parse_id(id)

    base =
      @pos_prior
      |> Map.get(pos, 0.94)
      |> apply_phrase_fallback_adjustment(pos, tag)
      |> clamp(@min_prior, @max_prior)

    # Hook for future tweaks (intent-aware boosts, idioms, etc.)
    base
    |> maybe_boost_greeting_phrase(lemma, pos, tag, ctx)
  end

  # ─── Internals ──────────────────────────────────────────────────────────────

  @spec parse_id(id()) :: {String.t(), String.t(), String.t()}
  defp parse_id(id) do
    case String.split(id, "|") do
      [lemma, pos, tag] -> {lemma, pos, tag}
      [lemma, pos] -> {lemma, pos, ""}
      [lemma] -> {lemma, "", ""}
      _ -> {id, "", ""}
    end
  end

  # Phrase fallbacks (e.g. "hey how is|phrase|fallback") get a *slight* extra
  # bump over other phrases, so in perfectly flat slates the MWE tends to win.
  defp apply_phrase_fallback_adjustment(base, "phrase", "fallback") do
    base + 0.02
  end

  defp apply_phrase_fallback_adjustment(base, _pos, _tag) do
    base
  end

  # Very simple greeting heuristic: if we later have phrase senses for things
  # like "how are you" or "hey how is", we can nudge them up a bit more.
  #
  # For now this is intentionally mild and only kicks in for phrase fallbacks
  # that look like greetings.
  @greeting_lemmas MapSet.new([
                     "hey how is",
                     "how are you",
                     "how is life treating you"
                   ])

  defp maybe_boost_greeting_phrase(base, lemma, "phrase", "fallback", _ctx) do
    if MapSet.member?(@greeting_lemmas, lemma) do
      base + 0.03
    else
      base
    end
    |> clamp(@min_prior, @max_prior)
  end

  defp maybe_boost_greeting_phrase(base, _lemma, _pos, _tag, _ctx) do
    base
  end

  defp clamp(value, low, high) when is_number(value) do
    value
    |> max(low)
    |> min(high)
  end
end
