defmodule Core.Semantic.AssertionBoundary do
  @moduledoc """
  P-040: the epistemic boundary for user-attributed assertions, in one place.

  Before P-040 three modules decided independently, with three different
  word lists, whether a sentence may become an attributed assertion
  (COLLAB §36.2 B3). Each profile below reproduces one of them *verbatim*:

    * `:svo_v1` — `Core.Semantic.SVOEvent.admissible_text?/1`
    * `:spatial_v1` — `Core.Semantic.ParticipantSpatial.admissible?/1`
    * `:episodic_prefix_v1` — the hypothetical/quote gate in
      `Core.Semantic.EpisodicEvents.statement/1` (before `parse_assertion/1`)

  The profiles genuinely disagree (for example on "no", "probably" and
  "allegedly"). `disagreements/0` makes that visible. Choosing one canonical
  profile is a semantic decision for Bradley/GPT-6, not a refactor, so no
  profile is preferred here.

  Admissible means only "not blocked by this boundary". It never means
  true, verified, or safe to persist. Pure; never raises.
  """

  @profiles [:svo_v1, :spatial_v1, :episodic_prefix_v1]

  @blocked_punctuation ["?", "\"", "“", "”", ";", ":", "\n"]

  # Word alternations, in the exact order of the original regex literals.
  @svo_words ~w(not never no maybe perhaps possibly probably might could would should
                if unless apparently supposedly reportedly)
  @svo_prefixes ["i think", "i guess", "imagine", "suppose", "according to", "remember that"]

  @spatial_words ~w(not never maybe perhaps possibly might could would should if unless
                    apparently reportedly allegedly supposedly)

  @episodic_prefixes ["if", "maybe", "perhaps", "suppose", "imagine", "i think", "i guess",
                      "what if", "could", "would", "please", "remember that"]

  @doc "The supported profile names."
  @spec profiles() :: [atom()]
  def profiles, do: @profiles

  @doc "True when `text` is not blocked by `profile`'s assertion boundary."
  @spec admissible?(term(), atom()) :: boolean()
  def admissible?(text, :svo_v1) when is_binary(text) do
    t = String.trim(text)

    t != "" and byte_size(t) <= 256 and
      not String.contains?(t, @blocked_punctuation) and
      not Regex.match?(word_regex(@svo_words), t) and
      not Regex.match?(prefix_regex(@svo_prefixes), t)
  end

  def admissible?(text, :spatial_v1) when is_binary(text) do
    String.length(text) in 1..256 and
      not String.contains?(text, @blocked_punctuation) and
      not Regex.match?(word_regex(@spatial_words), text)
  end

  def admissible?(raw, :episodic_prefix_v1) when is_binary(raw) do
    s = raw |> String.trim() |> String.trim_trailing(".")
    s = Regex.replace(~r/^(?:actually[, ]+|to be clear[, ]+)/iu, s, "")

    not (Regex.match?(prefix_regex(@episodic_prefixes), s) or String.contains?(s, ~s(")))
  end

  def admissible?(_, _), do: false

  @doc """
  Every listed word on which the profiles disagree.

  Each entry is `%{word: w, anywhere: [profile], sentence_initial: [profile]}`:
  `anywhere` profiles block the word in any position; `sentence_initial`
  profiles block it only as a sentence-initial phrase.
  """
  @spec disagreements() :: [map()]
  def disagreements do
    lexical = %{svo_v1: @svo_words, spatial_v1: @spatial_words, episodic_prefix_v1: []}
    initial = %{svo_v1: @svo_prefixes, spatial_v1: [], episodic_prefix_v1: @episodic_prefixes}

    words =
      (@svo_words ++ @spatial_words ++ @svo_prefixes ++ @episodic_prefixes)
      |> Enum.uniq()
      |> Enum.sort()

    words
    |> Enum.map(fn word ->
      %{
        word: word,
        anywhere: for(p <- @profiles, word in lexical[p], do: p),
        sentence_initial: for(p <- @profiles, word in initial[p], do: p)
      }
    end)
    |> Enum.reject(fn %{anywhere: a, sentence_initial: s} ->
      length(a) == length(@profiles) and s == []
    end)
  end

  defp word_regex(words),
    do: Regex.compile!("\\b(?:" <> Enum.join(words, "|") <> ")\\b", "iu")

  defp prefix_regex(phrases),
    do: Regex.compile!("^(?:" <> Enum.join(phrases, "|") <> ")\\b", "iu")
end
