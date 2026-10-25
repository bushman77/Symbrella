defmodule Db.JSONL.IO do
  @moduledoc false

  @default_filename "english.jsonl"

  # Commands run from the umbrella root. Never fall back to an app-local
  # corpus with a different version of the dictionary.
  @spec default_paths() :: [String.t()]
  def default_paths do
    [Path.expand(Path.join("priv", @default_filename))]
  end

  @spec default_path() :: String.t()
  def default_path, do: hd(default_paths())

  @spec head(non_neg_integer(), keyword()) :: {:ok, [binary()]} | {:error, any()}
  def head(n \\ 3, opts \\ []) when is_integer(n) and n >= 0 do
    path = resolve_path(opts)

    if File.exists?(path) do
      {:ok, path |> stream_lines() |> Enum.take(n)}
    else
      {:error, {:no_such_file, path}}
    end
  end

  @spec resolve_path(keyword()) :: String.t()
  def resolve_path(opts) do
    case Keyword.get(opts, :path) do
      nil -> default_path()
      p -> Path.expand(p)
    end
  end

  @spec stream_lines(String.t()) :: Enumerable.t()
  def stream_lines(path) do
    path
    |> File.stream!(:line, [])
    |> Stream.map(&trim_newline/1)
    |> Stream.reject(&(&1 == ""))
  end

  @spec jason_available?() :: boolean()
  def jason_available? do
    Code.ensure_loaded?(Jason) and function_exported?(Jason, :decode, 1) and
      function_exported?(Jason, :encode, 1)
  end

  @spec needles(String.t(), String.t()) :: {String.t(), String.t()}
  def needles(field, word) do
    encoded = encode_json_string(word)
    {~s("#{field}":#{encoded}), ~s("#{field}": #{encoded})}
  end

  defp trim_newline(line) do
    line
    |> String.trim_trailing("\n")
    |> String.trim_trailing("\r")
  end

  defp encode_json_string(word) do
    if Code.ensure_loaded?(Jason) and function_exported?(Jason, :encode, 1) do
      case Jason.encode(word) do
        {:ok, s} -> s
        _ -> ~s("#{escape_json(word)}")
      end
    else
      ~s("#{escape_json(word)}")
    end
  end

  defp escape_json(s) do
    s
    |> String.replace("\\", "\\\\")
    |> String.replace("\"", "\\\"")
    |> String.replace("\n", "\\n")
    |> String.replace("\r", "\\r")
    |> String.replace("\t", "\\t")
  end
end
