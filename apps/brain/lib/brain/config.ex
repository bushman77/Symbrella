defmodule Brain.Config do
  @moduledoc """
  Runtime configuration helpers for Brain.

  Notably: assistant identity (name/norm/aliases) used across regions and UI-adjacent
  semantics (e.g., treating the assistant name as a system entity rather than a
  dictionary lookup).
  """

  @default_name "Symbrella"

  @type assistant :: %{
          name: String.t(),
          norm: String.t(),
          aliases: [String.t()]
        }

  @spec assistant() :: assistant()
  def assistant do
    cfg0 = Application.get_env(:brain, :assistant, [])

    cfg =
      cond do
        is_map(cfg0) -> Map.to_list(cfg0)
        is_list(cfg0) -> cfg0
        true -> []
      end

    name =
      cfg
      |> Keyword.get(:name, @default_name)
      |> to_string()
      |> String.trim()
      |> case do
        "" -> @default_name
        v -> v
      end

    norm =
      cfg
      |> Keyword.get(:norm)
      |> case do
        v when is_binary(v) ->
          v1 = String.trim(v)
          if v1 == "", do: norm_text(name), else: norm_text(v1)

        _ ->
          norm_text(name)
      end

    aliases =
      cfg
      |> Keyword.get(:aliases, [])
      |> List.wrap()
      |> Enum.map(&to_string/1)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.map(&norm_text/1)
      |> Enum.uniq()

    %{
      name: name,
      norm: norm,
      aliases: aliases
    }
  end

  @spec assistant_match?(map() | String.t() | atom() | nil) :: boolean()
  def assistant_match?(nil), do: false

  def assistant_match?(%{} = tok) do
    v =
      Map.get(tok, :norm) ||
        Map.get(tok, "norm") ||
        Map.get(tok, :phrase) ||
        Map.get(tok, "phrase") ||
        Map.get(tok, :text) ||
        Map.get(tok, "text") ||
        Map.get(tok, :word) ||
        Map.get(tok, "word")

    assistant_match?(v)
  end

  def assistant_match?(v) when is_atom(v), do: assistant_match?(Atom.to_string(v))

  def assistant_match?(v) when is_binary(v) do
    a = assistant()
    n = norm_text(v)
    n == a.norm or n in a.aliases
  end

  def assistant_match?(_), do: false

  defp norm_text(v) when is_binary(v) do
    v
    |> String.downcase()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end
end

