defmodule Brain.LIFG.Stage1.Boundary do
  @moduledoc """
  Boundary and chargram validation for Stage1 tokens.

  Checks whether tokens align with the source sentence at their
  declared span, rejects chargram-sourced tokens, and emits
  telemetry for boundary drops.
  """

  alias Brain.Utils.Safe

  @type check_result :: :ok | {:error, :chargram | :nonword_edges | :span_mismatch}

  @spec check(binary() | nil, map(), binary(), boolean()) :: check_result()
  def check(sentence, tok, phrase_norm, token_mwe?) do
    source = Safe.get(tok, :source) || Safe.get(tok, "source")
    kind = Safe.get(tok, :kind) || Safe.get(tok, "kind")
    flag = Safe.get(tok, :chargram?, false) || Safe.get(tok, "chargram?", false)

    cond do
      source in [:chargram, :char_ngram, "chargram", "char_ngram"] or
        kind in [:chargram, :char_ngram, "chargram", "char_ngram"] or flag ->
        {:error, :chargram}

      phrase_nil_or_empty?(phrase_norm) ->
        {:error, :chargram}

      token_mwe? ->
        check_mwe(sentence, tok, phrase_norm)

      true ->
        check_unigram(sentence, tok, phrase_norm)
    end
  end

  def handle_chargram_drop(tok, tok_index, token_phrase, token_mwe?, acc, ctx) do
    tele_phrase = telemetry_phrase(tok, token_phrase)

    :telemetry.execute(
      ctx.chargram_event,
      %{},
      %{
        token_index: tok_index,
        phrase: tele_phrase,
        mw: token_mwe?,
        reason: :chargram,
        count: 1,
        frame_seq: ctx.frame_seq,
        frame_ts_ms: ctx.frame_ts_ms,
        frame_run_id: ctx.frame_run_id,
        v: 2
      }
    )

    acc
    |> Map.update!(:rejected, &[tok_index | &1])
    |> Map.update!(:chargram, &(&1 + 1))
  end

  def handle_boundary_drop(_tok, tok_index, token_phrase, token_mwe?, reason, acc, ctx) do
    :telemetry.execute(
      ctx.boundary_event,
      %{},
      %{
        token_index: tok_index,
        phrase: token_phrase,
        mw: token_mwe?,
        reason: reason,
        count: 1,
        frame_seq: ctx.frame_seq,
        frame_ts_ms: ctx.frame_ts_ms,
        frame_run_id: ctx.frame_run_id,
        v: 2
      }
    )

    acc
    |> Map.update!(:rejected, &[tok_index | &1])
    |> Map.update!(:boundary_drops, &(&1 + 1))
  end

  # ===== Internal checks =====

  defp check_mwe(sentence, tok, phrase_norm) do
    span = tok_span(tok)

    case {sentence, span} do
      {s, {start, stop}} when is_binary(s) ->
        if span_sub_norm(s, start, stop) == phrase_norm do
          :ok
        else
          {:error, :chargram}
        end

      _ ->
        if phrase_valid_mwe?(phrase_norm), do: :ok, else: {:error, :chargram}
    end
  end

  defp check_unigram(sentence, tok, phrase_norm) do
    span = tok_span(tok)

    case {sentence, span} do
      {s, {start, stop}} when is_binary(s) ->
        size = byte_size(s)
        sub_norm = span_sub_norm(s, start, stop)

        cond do
          sub_norm != phrase_norm ->
            cond do
              String.contains?(phrase_norm, " ") -> {:error, :chargram}
              phrase_valid_unigram?(phrase_norm) or phrase_valid_mwe?(phrase_norm) -> {:error, :span_mismatch}
              true -> {:error, :chargram}
            end

          true ->
            left_ok = start == 0 or not word_byte?(byte_at(s, start - 1))
            right_ok = stop == size or not word_byte?(byte_at(s, stop))
            if left_ok and right_ok, do: :ok, else: {:error, :nonword_edges}
        end

      _ ->
        if phrase_valid_unigram?(phrase_norm) or phrase_valid_mwe?(phrase_norm) do
          :ok
        else
          {:error, :chargram}
        end
    end
  end

  # ===== Span helpers =====

  defp tok_span(tok) do
    case Safe.get(tok, :span) || Safe.get(tok, "span") do
      {s, e} when is_integer(s) and is_integer(e) and s >= 0 and e > s -> {s, e}
      [s, e] when is_integer(s) and is_integer(e) and s >= 0 and e > s -> {s, e}
      _ -> nil
    end
  end

  defp span_sub_norm(sentence, start, stop) do
    try do
      cond do
        not is_binary(sentence) or not is_integer(start) or not is_integer(stop) -> ""
        start < 0 or stop <= start or stop > byte_size(sentence) -> ""
        true -> binary_part(sentence, start, stop - start) |> norm()
      end
    rescue
      _ -> ""
    end
  end

  defp byte_at(bin, idx) when is_binary(bin) and is_integer(idx) do
    if idx >= 0 and idx < byte_size(bin), do: :binary.at(bin, idx), else: nil
  end

  defp word_byte?(nil), do: false
  defp word_byte?(c) when is_integer(c) do
    (c >= ?0 and c <= ?9) or (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z) or c == ?_
  end

  # ===== Phrase shape helpers =====

  defp phrase_nil_or_empty?(nil), do: true
  defp phrase_nil_or_empty?(""), do: true
  defp phrase_nil_or_empty?(p) when is_binary(p), do: String.trim(p) == ""
  defp phrase_nil_or_empty?(_), do: false

  defp phrase_valid_unigram?(phrase) when is_binary(phrase) do
    clean = phrase |> String.downcase() |> String.replace(~r/[^[:alnum:]]/u, "") |> String.trim()
    clean != "" and not String.contains?(clean, " ")
  end
  defp phrase_valid_unigram?(_), do: false

  defp phrase_valid_mwe?(phrase) when is_binary(phrase) do
    tokens = phrase |> String.downcase() |> String.trim() |> String.split(~r/\s+/, trim: true)
    length(tokens) >= 2 and Enum.all?(tokens, &phrase_valid_unigram?/1)
  end
  defp phrase_valid_mwe?(_), do: false

  # ===== Shared utilities =====

  defp telemetry_phrase(tok, token_phrase) do
    Safe.get(tok, :lemma) || Safe.get(tok, "lemma") ||
      Safe.get(tok, :word) || Safe.get(tok, "word") || token_phrase
  end

  defp norm(nil), do: ""
  defp norm(v) when is_binary(v) do
    v |> String.downcase() |> String.trim()
    |> String.replace(~r/^\p{P}+/u, "")
    |> String.replace(~r/\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end
  defp norm(v), do: v |> to_string() |> norm()
end
