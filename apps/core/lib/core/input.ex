defmodule Core.Input do
  @moduledoc """
  Small, centralized accessors for Core application defaults.
  """

  @doc """
  Tokenizer defaults with a safe, in-module fallback.

  Merges application config (if any) onto base defaults.
  """
  @spec tokenizer_defaults() :: %{mode: atom(), emit_chargrams: boolean()}
  def tokenizer_defaults do
    base = %{mode: :words, emit_chargrams: false}

    case Application.get_env(:core, :tokenizer_defaults) do
      nil -> base
      kw when is_list(kw) -> Map.merge(base, Map.new(kw))
      m when is_map(m) -> Map.merge(base, m)
      _ -> base
    end
  end
end

