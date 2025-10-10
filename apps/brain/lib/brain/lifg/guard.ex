defmodule Brain.LIFG.Guard do
  @moduledoc ~S"""
  Compatibility shim for Core↔LIFG token intake.

  Responsibilities (structural only — no semantics):
    • Mapify structs and loose inputs.
    • Ensure every token has a stable integer :index.
    • Normalize optional :span to the project’s canonical shape {start, len}.
    • Sort by span start **iff** every token has a valid span; otherwise keep input order.

  NOTE: Boundary checks and char-gram drops live in BoundaryGuard.
  """

  @type token :: %{
          optional(:index) => non_neg_integer(),
          optional(:span) => {non_neg_integer(), non_neg_integer()},
          optional(:phrase) => String.t(),
          optional(:mw) => boolean(),
          optional(:n) => pos_integer()
        }

  @spec sanitize(list()) :: list()
  def sanitize(tokens) when is_list(tokens) do
    tokens
    |> Enum.map(&mapify/1)
    |> ensure_indexed()
    |> ensure_phrase()
    |> normalize_spans()
    |> sort_by_span_if_all_valid()
  end

  # -------- internals --------------------------------------------------

  # IMPORTANT: match structs first; structs are maps and would match %{} otherwise
  defp mapify(%_{} = s), do: Map.from_struct(s)
  defp mapify(%{} = t), do: t
  defp mapify(other), do: %{phrase: to_string(other)}

  defp ensure_indexed(list) do
    list
    |> Enum.with_index()
    |> Enum.map(fn {t, i} ->
      idx =
        Map.get(t, :index) ||
          Map.get(t, "index") ||
          i

      Map.put(t, :index, idx)
    end)
  end

  defp ensure_phrase(list) do
    Enum.map(list, fn t ->
      phrase =
        Map.get(t, :phrase) ||
          Map.get(t, "phrase") ||
          ""

      Map.put(t, :phrase, to_string(phrase))
    end)
  end

  # Accept both {start, end} and {start, len}; convert to {start, len} (len > 0)
  defp normalize_spans(list) do
    Enum.map(list, fn t ->
      case Map.get(t, :span) || Map.get(t, "span") do
        {s, e} when is_integer(s) and is_integer(e) ->
          phrase_len = t |> Map.get(:phrase, "") |> to_string() |> String.length()

          len =
            cond do
              # Looks like {start, end}; turn into length, prefer exact match when possible
              e > s and phrase_len > 0 and e - s == phrase_len -> e - s
              e > s and phrase_len == 0 -> e - s
              # Otherwise treat as already-length
              e > 0 -> e
              true -> 0
            end

          if len > 0 do
            Map.put(t, :span, {s, len})
          else
            Map.delete(t, :span)
          end

        _ ->
          Map.delete(t, :span)
      end
    end)
  end

  defp sort_by_span_if_all_valid(list) do
    if Enum.all?(list, &valid_span?/1) do
      Enum.sort_by(list, fn t ->
        {elem(Map.fetch!(t, :span), 0), Map.fetch!(t, :index)}
      end)
    else
      list
    end
  end

  defp valid_span?(%{span: {s, l}})
       when is_integer(s) and is_integer(l) and s >= 0 and l > 0,
       do: true

  defp valid_span?(_), do: false
end

