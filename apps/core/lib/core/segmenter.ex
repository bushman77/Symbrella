defmodule Core.Segmenter do
  @moduledoc """
  All-combos (n-gram) segmenter: assume every chunk exists.

  - Input should be **normalized** before calling.
  - Produces **every** contiguous phrase (n-gram) with length 1..`max_len`,
    at every start position (left→right).
  - Each segment is marked `source: :assumed` and `instances: []`.
  - No DB/registry/remote calls here — purely combinatorial.

  ## Examples

      iex> Core.Segmenter.segment_phrases("Hello there")
      [
        %{text: "Hello there", start: 0,  stop: 11, mw?: true,  source: :assumed, instances: []},
        %{text: "Hello",       start: 0,  stop: 5,  mw?: false, source: :assumed, instances: []},
        %{text: "there",       start: 6,  stop: 11, mw?: false, source: :assumed, instances: []}
      ]

      iex> Core.Segmenter.segment_phrases("Kick the bucket today", max_len: 3)
      [
        %{text: "Kick the bucket", start: 0,  stop: 15, mw?: true,  source: :assumed, instances: []},
        %{text: "Kick the",        start: 0,  stop: 9,  mw?: true,  source: :assumed, instances: []},
        %{text: "Kick",            start: 0,  stop: 4,  mw?: false, source: :assumed, instances: []},
        %{text: "the bucket today",start: 5,  stop: 21, mw?: true,  source: :assumed, instances: []},
        %{text: "the bucket",      start: 5,  stop: 15, mw?: true,  source: :assumed, instances: []},
        %{text: "the",             start: 5,  stop: 8,  mw?: false, source: :assumed, instances: []},
        %{text: "bucket today",    start: 9,  stop: 21, mw?: true,  source: :assumed, instances: []},
        %{text: "bucket",          start: 9,  stop: 15, mw?: false, source: :assumed, instances: []},
        %{text: "today",           start: 16, stop: 21, mw?: false, source: :assumed, instances: []}
      ]
  """

  @type segment :: %{
          text: String.t(),
          start: non_neg_integer(),
          stop: non_neg_integer(),
          mw?: boolean(),
          source: :assumed,
          instances: [String.t()]
        }

  @doc """
  Emit all contiguous chunks (n-grams) up to `max_len`.

  Options:
    * `:max_len` — maximum words per chunk (default: dynamic cap `min(5, word_count)`).

  Returns segments ordered by **start asc** and, for the same start, by **length desc**.
  """
  @spec segment_phrases(String.t(), keyword()) :: [segment]
  def segment_phrases(sentence, opts \\ []) when is_binary(sentence) do
    words = word_spans(sentence)
    wc    = length(words)
    cap   = Keyword.get(opts, :max_len, min(5, wc))

    all_chunks(words, cap, sentence)
  end

  # ——— Private, pure helpers ———

  @typep span :: %{text: String.t(), start: non_neg_integer(), stop: non_neg_integer()}

  # Split into non-space runs, preserving byte spans.
  @spec word_spans(String.t()) :: [span]
  defp word_spans(sentence) do
    Regex.scan(~r/\S+/u, sentence, return: :index)
    |> Enum.map(fn [{start, len}] ->
      %{
        text: binary_part(sentence, start, len),
        start: start,
        stop: start + len
      }
    end)
  end

  # Build a single segment from words[i..i+k-1].
  @spec build_segment([span], non_neg_integer(), pos_integer(), String.t()) :: map()
  defp build_segment(words, i, k, sentence) do
    first = Enum.at(words, i)
    last  = Enum.at(words, i + k - 1)
    start = first.start
    stop  = last.stop

    %{
      text: binary_part(sentence, start, stop - start),
      start: start,
      stop: stop,
      mw?: k > 1,
      source: :assumed,
      instances: []
    }
  end

  # Emit all chunks starting at each position, producing final order directly:
  # start asc; within a start, length desc. No warnings, no ++ in loop.
  @spec all_chunks([span], pos_integer(), String.t()) :: [map()]
  defp all_chunks(words, cap, sentence), do: all_chunks(words, cap, sentence, [])

  defp all_chunks([], _cap, _sentence, acc_rev), do: Enum.reverse(acc_rev)

  defp all_chunks(words, cap, sentence, acc_rev) do
    maxk = min(cap, length(words))

    # Build chunks for this start in short->long order…
    here_short_first =
      for k <- 1..maxk do
        build_segment(words, 0, k, sentence)
      end

    # …prepend them so that final reverse yields long->short at this start
    acc_rev2 = Enum.reduce(here_short_first, acc_rev, fn seg, acc -> [seg | acc] end)

    all_chunks(tl(words), cap, sentence, acc_rev2)
  end
end

