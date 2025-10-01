defmodule Db.Episode do
  @moduledoc """
  Ecto schema for hippocampus episodes (persistent slates).

  Notes
  -----
  • `embedding` uses `Pgvector.Ecto.Vector`; during cast it becomes a `%Pgvector{}`.
  • We validate that embedding is either a list or `%Pgvector{}` and that its length
    matches `@embedding_dim`.
  • `token_count` is derived from the (possibly updated) `:tokens` field so it stays
    consistent even if callers forget to set it.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{}

  @embedding_dim Application.compile_env(:db, :embedding_dim, 1536)

  schema "episodes" do
    field :user_id, :binary_id
    field :tokens, {:array, :string}, default: []
    field :token_count, :integer, default: 0
    field :si, :map, default: %{}
    field :embedding, Pgvector.Ecto.Vector
    field :tags, {:array, :string}, default: []
    timestamps(type: :naive_datetime_usec)
  end

  @doc """
  Build a changeset for an episode.
  - Ensures `:tokens` is an array of strings (coerces with `to_string/1`).
  - Derives `:token_count` from `:tokens`.
  - Validates `:embedding` length for both list and `%Pgvector{}`.
  """
  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(ep, attrs) do
    ep
    |> cast(attrs, [:user_id, :tokens, :token_count, :si, :tags, :embedding])
    |> validate_required([:tokens, :si])
    |> normalize_tokens()
    |> put_token_count()
    |> validate_embedding_shape()
  end

  # --- Helpers ---------------------------------------------------------------

  # Ensure tokens is a list of strings. Do not lowercase here (that lives in the
  # context) to keep this schema side-effect free. Safe to call even if nil.
  defp normalize_tokens(changeset) do
    case get_change(changeset, :tokens) do
      nil -> changeset
      list when is_list(list) ->
        put_change(changeset, :tokens, Enum.map(list, &to_string/1))
      other ->
        add_error(changeset, :tokens, "must be a list of strings, got: #{inspect(other)}")
    end
  end

  defp put_token_count(changeset) do
    tokens = get_field(changeset, :tokens) || []
    put_change(changeset, :token_count, length(tokens))
  end

  defp validate_embedding_shape(changeset) do
    val = get_change(changeset, :embedding, get_field(changeset, :embedding))

    cond do
      is_nil(val) ->
        changeset

      match?(%Pgvector{}, val) ->
        case Pgvector.to_list(val) do
          list when is_list(list) and length(list) == @embedding_dim -> changeset
          list when is_list(list) -> add_error(changeset, :embedding, "embedding length must be #{@embedding_dim} (got #{length(list)})")
          _ -> add_error(changeset, :embedding, "invalid pgvector value")
        end

      is_list(val) ->
        if length(val) == @embedding_dim do
          changeset
        else
          add_error(changeset, :embedding, "embedding length must be #{@embedding_dim} (got #{length(val)})")
        end

      true ->
        add_error(changeset, :embedding, "embedding must be a list of floats or a %Pgvector{}")
    end
  end
end

