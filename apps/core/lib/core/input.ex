defmodule Core.Input do
  @moduledoc false

  @doc """
  Default tokenizer settings used across Core unless explicitly overridden.
  Tests expect `:mode` to default to `:words`.
  """
  @spec tokenizer_defaults() :: map()
  def tokenizer_defaults do
    %{
      mode: :words,
      # Keep char-grams OFF unless explicitly requested by opts or tests.
      chargram: false,
      ngram: 1,
      keep_punct: false
    }
  end

  # Common legit short words we should never treat as char-grams.
  @default_short_whitelist MapSet.new(~w(
    i a we to of in on at by or it my do go be us up an am me he if is as so no
    ai ok hi oh uh um ya yo id im i'm i’m
  ))

  defp short_whitelist do
    extra = Application.get_env(:core, :chargram_whitelist, [])
    MapSet.union(@default_short_whitelist, MapSet.new(Enum.map(extra, &String.downcase/1)))
  end

  @doc """
  Drop char-gram tokens from a SemanticInput or a raw token list.

  A token is considered a char-gram if any of:
    * source/kind == :chargram or "chargram"
    * `chargram` flag is true/"true"
    * (optionally) super-short fragment **not** in whitelist AND not an MWE head

  We avoid dropping legit short words like "I", "we", "to", or "im".
  """
  @spec drop_chargrams(%{tokens: list()} | list()) :: %{tokens: list()} | list()
  def drop_chargrams(%{tokens: tokens} = si), do: %{si | tokens: drop_chargrams(tokens)}

  def drop_chargrams(tokens) when is_list(tokens) do
    wl = short_whitelist()

    tokens
    |> Enum.reject(fn t ->
      src = Map.get(t, :source) || Map.get(t, "source")
      kind = Map.get(t, :kind) || Map.get(t, "kind")
      charflag = Map.get(t, :chargram) || Map.get(t, "chargram")
      phrase0 = (Map.get(t, :phrase) || Map.get(t, "phrase") || "") |> to_string()
      phrase = String.downcase(phrase0)
      mw? = Map.get(t, :mw) || Map.get(t, "mw") || false

      # shortish = length <= 2 once whitespace removed
      shortish =
        phrase0
        |> String.replace(~r/\s+/u, "")
        |> String.length()
        |> Kernel.<=() >
          2
          |> case do
            :lt -> true
            :eq -> true
            _ -> false
          end

      flagged_chargram =
        src in [:chargram, "chargram"] or
          kind in [:chargram, "chargram"] or
          charflag in [true, "true"]

      short_fragment =
        shortish and not mw? and not MapSet.member?(wl, phrase)

      flagged_chargram or short_fragment
    end)
    |> Enum.with_index()
    |> Enum.map(fn {t, i} ->
      # Preserve existing :index if present; else assign stable index
      case Map.fetch(t, :index) do
        {:ok, _} -> t
        :error -> Map.put(t, :index, i)
      end
    end)
  end

  def drop_chargrams(other), do: other
end
