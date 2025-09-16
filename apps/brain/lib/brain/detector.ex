defmodule Brain.Detector do
  @moduledoc """
  Detect running braincells for given tokens, *before* touching the DB.
  We derive candidate IDs as: "norm|pos|type", then `Registry.lookup/2`.
  POS list is configurable via `config :symbrella, :pos_inventory`.
  """
  @default_pos ~w(noun verb adj adv pron det adp num cconj sconj part propn punct intj sym x)

  @spec running_ids_for_tokens([Core.Token.t()]) :: [String.t()]
  def running_ids_for_tokens(tokens) when is_list(tokens) do
    pos_list =
      Application.get_env(:symbrella, :pos_inventory) || @default_pos

    tokens
    |> Enum.flat_map(fn %{phrase: phrase, mw: mw?} ->
      norm = normize(phrase)
      type = if mw?, do: "phrase", else: "word"

      for pos <- pos_list do
        id = Enum.join([norm, pos || "", type, ""], "|")
        case Registry.lookup(Brain.Registry, id) do
          [] -> nil
          _ -> id
        end
      end
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
  end

  defp normize(p) do
    p
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/, " ")
  end
end
