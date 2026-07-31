defmodule Db.NativeVocabToken do
  @moduledoc """
  Persistent Symbrella-native vocabulary token.

  This table gives Symbrella stable token IDs for native training.

  Important:
  - These token IDs are Symbrella-native.
  - They are not pretrained model tokenizer IDs.
  - Once assigned, token IDs should remain stable.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{}

  @timestamps_opts [type: :naive_datetime_usec]

  schema "symbrella_vocab_tokens" do
    field(:token, :string)
    field(:token_id, :integer)
    field(:count, :integer, default: 0)
    field(:source, :string, default: "brain_cells")

    timestamps()
  end

  @doc false
  def changeset(vocab_token, attrs) do
    vocab_token
    |> cast(attrs, [:token, :token_id, :count, :source])
    |> validate_required([:token, :token_id, :count, :source])
    |> update_change(:token, &normalize_token/1)
    |> update_change(:source, &normalize_source/1)
    |> validate_number(:token_id, greater_than_or_equal_to: 0)
    |> validate_number(:count, greater_than_or_equal_to: 0)
    |> unique_constraint(:token)
    |> unique_constraint(:token_id)
    |> check_constraint(:token_id, name: :token_id_nonnegative)
    |> check_constraint(:count, name: :count_nonnegative)
  end

  defp normalize_token(token) when is_binary(token), do: String.trim(token)
  defp normalize_token(other), do: other

  defp normalize_source(source) when is_binary(source) do
    source
    |> String.trim()
    |> case do
      "" -> "brain_cells"
      value -> value
    end
  end

  defp normalize_source(other), do: other
end
