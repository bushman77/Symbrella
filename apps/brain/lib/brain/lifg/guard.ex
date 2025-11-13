# apps/brain/lib/brain/lifg/guard.ex
defmodule Brain.LIFG.Guard do
  @moduledoc ~S"""
  Compatibility shim for Core↔LIFG token intake (structural only).

  ✅ Guardrails-compliant: no references to Core or its structs.

  Responsibilities:
    • Accept flexible inputs: list, a single map/struct/string, or `%{tokens: ..., sentence: ...}`.
    • Mapify any struct generically (no module deps), keep everything as plain maps.
    • Ensure stable integer :index.
    • Canonicalize :span to **{start, stop}** (start-inclusive, stop-exclusive).
    • If a sentence is present and :span missing, best-effort hydrate from the first occurrence.
    • Sort by span start **iff** every token has a valid span; else keep input order.

  NOTE: Boundary/char-gram rules belong in BoundaryGuard (not here).
  """

  @type token_map :: %{
          optional(:index) => non_neg_integer(),
          optional(:span) => {non_neg_integer(), non_neg_integer()},
          optional(:phrase) => String.t(),
          optional(:mw) => boolean(),
          optional(:n) => pos_integer(),
          optional(:instances) => list()
        }

  @spec sanitize(
          nil
          | binary()
          | token_map
          | [token_map | struct()]
          | %{required(:tokens) => list(), optional(:sentence) => binary()}
        ) :: [token_map] | %{tokens: [token_map]}
  def sanitize(nil), do: []

  # SI map → return SI map with sanitized tokens (still plain maps)
  def sanitize(%{tokens: toks} = si) when is_list(toks) do
    sent = Map.get(si, :sentence)
    %{si | tokens: sanitize_tokens(toks, sent)}
  end

  # List → return sanitized list
  def sanitize(list) when is_list(list), do: sanitize_tokens(list, nil)

  # Single struct/map/string → wrap to list (struct handled generically)
  def sanitize(%_{} = struct_tok), do: sanitize([struct_tok])
  def sanitize(%{} = tok_map),     do: sanitize([tok_map])
  def sanitize(phrase) when is_binary(phrase), do: sanitize([%{phrase: phrase}])

  # Fallback
  def sanitize(_other), do: []

  # ── Internals ────────────────────────────────────────────────────────────────

  defp sanitize_tokens(list, sentence) do
    list
    |> Enum.map(&mapify/1)           # structs → maps, keep maps as-is
    |> ensure_phrase()               # always a binary :phrase
    |> ensure_defaults()             # mw/instances/n
    |> canonicalize_spans(sentence)  # to {start, stop}, or drop :span
    |> ensure_indexed()              # stable :index
    |> sort_by_span_if_all_valid()   # only if all spans are valid
  end

  # IMPORTANT: match any struct without naming its module (no compile-time deps)
  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = t),  do: t
  defp mapify(other),    do: %{phrase: to_string(other)}

  defp ensure_phrase(list) do
    Enum.map(list, fn t ->
      phrase = (t[:phrase] || t["phrase"] || "") |> to_string()
      Map.put(t, :phrase, phrase)
    end)
  end

  defp ensure_defaults(list) do
    Enum.map(list, fn t ->
      t
      |> Map.put_new(:mw, false)
      |> Map.put_new(:instances, [])
      |> Map.put_new_lazy(:n, fn ->
        t[:phrase]
        |> to_string()
        |> String.split(~r/\s+/, trim: true)
        |> length()
        |> max(1)
      end)
    end)
  end

  # Accept both {start, len} and {start, stop}; canonicalize to {start, stop}.
  # If :span missing and sentence provided, hydrate from first occurrence of phrase.
  defp canonicalize_spans(list, nil) do
    Enum.map(list, &to_start_stop/1)
  end

  defp canonicalize_spans(list, sentence) when is_binary(sentence) do
    Enum.map(list, fn t ->
      case t[:span] || t["span"] do
        {s, e} when is_integer(s) and is_integer(e) ->
          put_span_start_stop(t, s, e)

        _ ->
          phrase = t[:phrase] |> to_string()
          case :binary.match(sentence, phrase) do
            {pos, _len} ->
              s = pos
              e = pos + byte_size(phrase)
              put_span_start_stop(t, s, e)

            :nomatch ->
              Map.delete(t, :span)
          end
      end
    end)
  end

  defp to_start_stop(t) do
    case t[:span] || t["span"] do
      {s, e} when is_integer(s) and is_integer(e) and e > s ->
        put_span_start_stop(t, s, e)

      {s, len} when is_integer(s) and is_integer(len) and len > 0 ->
        put_span_start_stop(t, s, s + len)

      _ ->
        Map.delete(t, :span)
    end
  end

  defp put_span_start_stop(t, s, e), do: Map.put(t, :span, {s, e})

  defp ensure_indexed(list) do
    list
    |> Enum.with_index()
    |> Enum.map(fn {t, i} ->
      idx = t[:index] || t["index"] || i
      Map.put(t, :index, idx)
    end)
  end

  defp sort_by_span_if_all_valid(list) do
    if Enum.all?(list, &valid_span?/1) do
      Enum.sort_by(list, fn t ->
        {{s, _e}, idx} = {Map.fetch!(t, :span), Map.fetch!(t, :index)}
        {s, idx}
      end)
    else
      list
    end
  end

  defp valid_span?(%{span: {s, e}})
       when is_integer(s) and is_integer(e) and s >= 0 and e > s,
       do: true

  defp valid_span?(_), do: false
end

