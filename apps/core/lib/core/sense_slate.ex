defmodule Core.SenseSlate do
  @moduledoc ~S"""
  Build a sense slate for tokens so LIFG has a stable, indexed menu of candidates.

  Output shape:
    %{
      token_start_idx => [
        %{
          id: String.t(),          # e.g., "bank|noun|money"
          prior: number(),         # frequency/recency prior (0..1 or any score)
          score: number(),         # initial score (defaults to prior)
          features: map() | nil,   # optional semantic feature payload
          source: atom()           # :lexicon (default) or others
        },
        ...
      ],
      ...
    }

  Notes:
  - Keys are the token **start** indices (`span: {start, end}` with end exclusive).
  - You supply `fetch_senses: (phrase :: String.t() -> [map])`.
    Return items at least with `:id` and optional `:prior`/`:features`.
  - We de-duplicate by `:id` per token index and sort by descending `prior`.
  """

  @type token :: %{
          required(:phrase) => String.t(),
          required(:span) => {non_neg_integer, non_neg_integer},
          optional(:mw) => boolean(),
          optional(any) => any
        }

  @type candidate_in :: %{
          optional(:id) => String.t(),
          optional(:prior) => number(),
          optional(:features) => map(),
          optional(:source) => atom()
        }

  @type candidate_out :: %{
          required(:id) => String.t(),
          required(:prior) => number(),
          required(:score) => number(),
          optional(:features) => map(),
          required(:source) => atom()
        }

  @type slate :: %{optional(non_neg_integer) => [candidate_out]}

  @spec build([token], keyword()) :: slate
  def build(tokens, opts \\ []) when is_list(tokens) do
    fetch_senses =
      case Keyword.fetch(opts, :fetch_senses) do
        {:ok, fun} when is_function(fun, 1) -> fun
        _ -> raise ArgumentError, "Core.SenseSlate.build/2 requires :fetch_senses (arity 1)"
      end

    tokens
    |> Enum.reduce(%{}, fn
      %{phrase: phrase, span: {s, _e}}, acc when is_binary(phrase) and is_integer(s) ->
        senses = fetch_senses.(phrase) || []

        normalized =
          senses
          |> Enum.map(&normalize_candidate/1)
          |> Enum.reject(&is_nil/1)

        Map.update(acc, s, normalized, fn old -> old ++ normalized end)

      _t, acc ->
        acc
    end)
    |> Enum.into(%{}, fn {k, list} ->
      list1 = Enum.uniq_by(list, & &1.id)
      list2 = Enum.sort_by(list1, fn c -> {-1.0 * c.prior, c.id} end)
      {k, list2}
    end)
  end

  @doc """
  Convenience to attach a slate to an SI map/struct:
    si = Core.SenseSlate.attach(si, slate)
  """
  @spec attach(map() | struct(), slate) :: map() | struct()
  def attach(si, slate) when is_map(si) and is_map(slate) do
    Map.put(si, :sense_candidates, slate)
  end

  # ---- helpers ----

  @spec normalize_candidate(candidate_in) :: candidate_out | nil
  defp normalize_candidate(%{} = c) do
    id = Map.get(c, :id)
    prior = Map.get(c, :prior, 0.0)
    features = Map.get(c, :features)
    source = Map.get(c, :source, :lexicon)

    cond do
      not is_binary(id) ->
        nil

      not is_number(prior) ->
        nil

      true ->
        %{
          id: id,
          prior: prior * 1.0,
          score: prior * 1.0,
          features: features,
          source: source
        }
    end
  end

  defp normalize_candidate(_), do: nil
end
