defmodule Db.JSONL.Util do
  @moduledoc false

  @spec ensure_map(any()) :: map()
  def ensure_map(%{} = m), do: m
  def ensure_map(_), do: %{}

  @spec take_limit(Enumerable.t(), non_neg_integer() | :infinity) :: Enumerable.t()
  def take_limit(stream, :infinity), do: stream
  def take_limit(stream, n) when is_integer(n) and n > 0, do: Stream.take(stream, n)
  def take_limit(_stream, 0), do: Stream.take([], 0)

  @spec normalize_word(any()) :: String.t()
  def normalize_word(nil), do: ""

  def normalize_word(s) when is_binary(s) do
    s |> String.downcase() |> String.trim() |> String.replace(~r/\s+/u, " ")
  end

  def normalize_word(other), do: other |> to_string() |> normalize_word()

  @spec normalize_tag(any()) :: String.t()
  def normalize_tag(t) do
    t
    |> to_string()
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  @spec normalize_atom(any()) :: String.t()
  def normalize_atom(s) do
    s
    |> to_string()
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  @spec flatten_term_list(any()) :: [String.t()]
  def flatten_term_list(list) do
    list
    |> List.wrap()
    |> Enum.map(fn
      %{"word" => w} -> w
      %{"term" => w} -> w
      %{"value" => w} -> w
      w when is_binary(w) -> w
      _ -> nil
    end)
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  @spec uniq_terms(any()) :: [String.t()]
  def uniq_terms(list) do
    list
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  @spec drop_self([String.t()], String.t()) :: [String.t()]
  def drop_self(list, word_norm) when is_list(list) do
    Enum.reject(list, fn t -> normalize_word(t) == word_norm end)
  end
end
