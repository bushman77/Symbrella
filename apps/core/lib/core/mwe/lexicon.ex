defmodule Core.MWE.Lexicon do
  @moduledoc """
  Batched, POS-preserving dictionary lookup for contiguous 2–5-word expressions.

  The lookup contributes lexical MWE hypotheses to `:mwe_matches` and stores
  their actual dictionary senses in `:mwe_lexicon`. It never changes base
  tokens or invents a dictionary sense when no entry exists.

  A caller can supply `:mwe_lookup` (a function of normalized phrase strings)
  for deterministic tests. In production one indexed DB query covers the
  bounded set of candidate spans.
  """

  import Ecto.Query, only: [from: 2]
  alias Core.MWE.Boundaries
  require Logger

  @min_n 2
  @max_n 5
  @max_terms 256

  @spec attach(map(), keyword()) :: map()
  def attach(si, opts \\ [])

  def attach(%{tokens: tokens} = si, opts) when is_list(tokens) and is_list(opts) do
    if Keyword.get(opts, :mwe_lexicon, :on) in [:off, false] do
      si
    else
      windows = windows(tokens, Keyword.get(opts, :mwe_max_n, @max_n))
      terms = windows |> Enum.map(& &1.phrase) |> Enum.uniq() |> Enum.take(@max_terms)

      if length(Enum.uniq_by(windows, & &1.phrase)) > @max_terms do
        Logger.warning("[core/mwe] capped lexical MWE lookup at #{@max_terms} expressions")
      end

      rows =
        case Keyword.get(opts, :mwe_lookup) do
          fun when is_function(fun, 1) -> fun.(terms)
          _ -> db_lookup(terms)
        end

      by_norm =
        rows
        |> List.wrap()
        |> Enum.filter(&is_map/1)
        |> Enum.group_by(fn row -> norm(field(row, :norm) || field(row, :word)) end)

      units =
        windows
        |> Enum.flat_map(fn window ->
          senses =
            Map.get(by_norm, window.phrase, [])
            |> Enum.map(&sense/1)
            |> Enum.filter(&(is_binary(&1.id) and is_binary(&1.pos)))

          if senses == [] do
            []
          else
            [Map.merge(window, %{
              source: :db_lexicon,
              senses: Enum.uniq_by(senses, & &1.id),
              lexical_pos: senses |> Enum.map(& &1.pos) |> Enum.uniq()
            })]
          end
        end)
        |> Enum.uniq_by(fn unit -> {unit.span, unit.phrase} end)

      matches =
        Enum.map(units, fn unit ->
          %{
            span: unit.span,
            size: unit.size,
            phrase: unit.phrase,
            kind: :lex,
            source: :db_lexicon,
            lexical_pos: unit.lexical_pos,
            sense_ids: Enum.map(unit.senses, & &1.id),
            pos: []
          }
        end)

      si
      |> Map.put(:mwe_lexicon, units)
      |> Map.update(:mwe_matches, matches, fn prior ->
        List.wrap(prior) ++ matches
      end)
    end
  end

  def attach(other, _opts), do: other

  defp windows(tokens, maximum) do
    max_n = if is_integer(maximum), do: min(maximum, @max_n), else: @max_n

    words =
      tokens
      |> Enum.filter(&(field(&1, :n) in [nil, 1]))
      |> Enum.with_index()
      |> Enum.map(fn {tok, index} ->
        %{
          index: field(tok, :index) || index,
          phrase: field(tok, :phrase),
          word: norm(field(tok, :phrase))
        }
      end)

    count = length(words)

    if count < @min_n or max_n < @min_n do
      []
    else
      for start <- 0..(count - @min_n),
          n <- @min_n..min(max_n, count - start),
          segment = Enum.slice(words, start, n),
          Enum.all?(segment, &(&1.word != "")),
          Boundaries.valid_window?(segment) do
        %{
          phrase: Enum.map_join(segment, " ", & &1.word),
          span: {hd(segment).index, List.last(segment).index},
          size: n
        }
      end
    end
  end

  defp db_lookup([]), do: []

  defp db_lookup(terms) do
    Db.all(
      from(c in Db.BrainCell,
        where: c.norm in ^terms,
        select: %{
          id: c.id,
          norm: c.norm,
          pos: c.pos,
          definition: c.definition,
          gram_function: c.gram_function,
          activation: c.activation
        }
      )
    )
  rescue
    exception ->
      Logger.warning("[core/mwe] lexical lookup unavailable: #{Exception.message(exception)}")
      []
  end

  defp sense(row) do
    %{
      id: field(row, :id),
      norm: norm(field(row, :norm) || field(row, :word)),
      pos: field(row, :pos),
      definition: field(row, :definition),
      gram_function: field(row, :gram_function) || [],
      activation: field(row, :activation)
    }
  end

  defp field(%{} = map, key),
    do: Map.get(map, key) || Map.get(map, Atom.to_string(key))

  defp field(_, _), do: nil

  defp norm(value) when is_binary(value) do
    value
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/^\p{P}+|\p{P}+$/u, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp norm(_), do: ""
end
