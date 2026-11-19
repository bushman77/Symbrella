defmodule Core.Semantic.MetalinguisticClaim do
  @moduledoc """
  A bounded speaker-attributed classification of an immediately preceding
  isolated *expression*. No dictionary sense, object identity or factual
  authority is established. This module is pure; Ledger.Owner alone admits
  the associated session records.
  """

  alias Core.Semantic.ProvisionalInterpretation

  @lexeme ~r/\A[\p{L}][\p{L}\p{M}'-]{0,39}\z/u
  @isolated ~r/\A\s*(?<term>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[!.]?\s*\z/u
  @classification ~r/\A\s*(?:it\s+is|it['’]s|its|that\s+is|that['’]s|this\s+is|this['’]s)\s+(?:a|an)\s+(?<class>[\p{L}][\p{L}\p{M}'-]{0,39})\s*[!.]?\s*\z/iu
  @window_ms 5 * 60 * 1000
  @source_fields [:kind, :status, :source_term, :source_turn_sha256,
                  :observed_at_ms, :seq, :factual_authority?]

  @doc "The only privacy-approved lexical marker: one word and a source hash."
  def isolated_marker(si, now_ms) when is_map(si) and is_integer(now_ms) and now_ms >= 0 do
    sentence = Map.get(si, :sentence)
    with true <- is_binary(sentence),
         true <- byte_size(sentence) <= 64,
         %{"term" => surface} <- Regex.named_captures(@isolated, sentence),
         term <- String.downcase(surface),
         true <- valid_term?(term),
         true <- no_attested_word_sense?(si, term),
         nil <- Map.get(si, :speech_act_frame),
         true <- no_object_semantics?(si),
         {:ok, %{status: :provisional, speech_act: :fragment,
                 operation: :ambiguous_topic, original_sentence: ^sentence,
                 target: %{lemma: ^term}, factual_authority?: false}} <-
           ProvisionalInterpretation.verified(si) do
      %{
        kind: :uninterpreted,
        status: :no_verified_semantics,
        source_term: term,
        source_turn_sha256: digest(sentence),
        observed_at_ms: now_ms,
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end
  def isolated_marker(_, _), do: nil

  @doc "Recheck bounded marker structure, timestamp and same-session sequence."
  def valid_source?(%{
        kind: :uninterpreted, status: :no_verified_semantics,
        source_term: term, source_turn_sha256: sha,
        observed_at_ms: at, seq: seq, factual_authority?: false
      }, now_ms)
      when is_binary(term) and is_binary(sha) and is_integer(at) and
           is_integer(seq) and is_integer(now_ms) and seq > 0 and at >= 0 do
    valid_term?(term) and term == String.downcase(term) and
      byte_size(sha) == 64 and Regex.match?(~r/\A[0-9a-f]{64}\z/, sha) and
      now_ms >= at and now_ms - at <= @window_ms
  end
  def valid_source?(_, _), do: false

  def source_fields, do: @source_fields

  @doc "Pure surface classification; never resolves an object-level pronoun."
  def detect(si) when is_map(si) do
    text = Map.get(si, :sentence)
    source = Map.get(si, :metalinguistic_source)
    with true <- is_binary(text) and byte_size(text) <= 160,
         true <- is_map(source),
         true <- valid_source?(source, Map.get(source, :observed_at_ms)),
         true <- Map.take(source, @source_fields) == source,
         nil <- Map.get(si, :speech_act_frame),
         true <- no_object_semantics?(si),
         %{"class" => surface} <- Regex.named_captures(@classification, text),
         class_term <- String.downcase(surface),
         true <- valid_term?(class_term) do
      %{
        kind: :metalinguistic_claim,
        status: :speaker_claimed,
        relation: :classification,
        source_term: source.source_term,
        source_turn_sha256: source.source_turn_sha256,
        source_seq: source.seq,
        class_term: class_term,
        current_turn_sha256: digest(text),
        epistemic: %{
          user_claimed: true,
          lexically_verified?: false,
          durably_persisted?: false
        },
        verification: :conversation_context_only,
        factual_authority?: false
      }
    else
      _ -> nil
    end
  end
  def detect(_), do: nil

  @doc "Optional observational attachment; disabled turns are left unchanged."
  def attach(si, opts \\ [])
  def attach(si, opts) when is_map(si) and is_list(opts) do
    if Keyword.get(opts, :metalinguistic_claims?, false) == true do
      Map.put(si, :metalinguistic_claim, detect(si))
    else
      si
    end
  end
  def attach(si, _), do: si

  @doc "Replay equality: an injected or stale claim cannot authorize admission."
  def verified(si) when is_map(si) do
    claim = Map.get(si, :metalinguistic_claim)
    case detect(si) do
      ^claim when is_map(claim) -> {:ok, claim}
      _ -> :not_applicable
    end
  end
  def verified(_), do: :not_applicable

  # V1 intentionally excludes dictionary-attested words, including named
  # entities and object-like nouns: "dog!" must not become a metalinguistic
  # reference merely because it is a single-token utterance.
  defp no_attested_word_sense?(si, term) do
    not Enum.any?(List.wrap(Map.get(si, :active_cells)), fn
      %{} = cell ->
        norm = Map.get(cell, :norm) || Map.get(cell, "norm")
        pos = Map.get(cell, :pos) || Map.get(cell, "pos")
        is_binary(norm) and (is_binary(pos) or is_atom(pos)) and
          String.downcase(norm) == term and pos not in [nil, "", :unknown, "unknown"]
      _ -> false
    end)
  end

  defp no_object_semantics?(si) do
    # Structural readings alone do not establish a referent. Independently
    # verified semantic readings do; those cannot be reinterpreted as a
    # speaker claim about a preceding *word*.
    case Map.get(si, :sentence_meaning) do
      %{status: status} when status in [:derived, :verified, :factually_verified] ->
        false
      _ -> true
    end
  end

  defp valid_term?(word) when is_binary(word) do
    byte_size(word) in 1..40 and Regex.match?(@lexeme, word)
  end
  defp valid_term?(_), do: false

  defp digest(text),
    do: :crypto.hash(:sha256, text) |> Base.encode16(case: :lower)
end
