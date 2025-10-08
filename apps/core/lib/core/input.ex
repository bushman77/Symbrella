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

  @doc """
  Drop char-gram tokens from a SemanticInput or a raw token list.

  A token is considered a char-gram if any of:
    * source/kind == :chargram or "chargram"
    * chargram flag true/"true"
    * its (whitespace-stripped) phrase length ≤ 2 (common fragment shape like "ck" or "t")
  """
  @spec drop_chargrams(%{tokens: list()} | list()) :: %{tokens: list()} | list()
  def drop_chargrams(%{tokens: tokens} = si), do: %{si | tokens: drop_chargrams(tokens)}
  def drop_chargrams(tokens) when is_list(tokens) do
    tokens
    |> Enum.reject(fn t ->
      src      = Map.get(t, :source) || Map.get(t, "source")
      kind     = Map.get(t, :kind) || Map.get(t, "kind")
      charflag = Map.get(t, :chargram) || Map.get(t, "chargram")
      phrase   = (Map.get(t, :phrase) || Map.get(t, "phrase") || "") |> to_string()
      shortish = phrase |> String.replace(" ", "") |> String.length() <= 2

      src in [:chargram, "chargram"] or
        kind in [:chargram, "chargram"] or
        charflag in [true, "true"] or
        shortish
    end)
    |> Enum.with_index()
    |> Enum.map(fn {t, i} -> Map.put_new(t, :index, Map.get(t, :index, i)) end)
  end

  def drop_chargrams(other), do: other
end

