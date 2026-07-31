defmodule Db.Repo.Migrations.CreateSymbrellaVocabTokens do
  use Ecto.Migration

  def change do
    create table(:symbrella_vocab_tokens) do
      add(:token, :text, null: false)
      add(:token_id, :integer, null: false)
      add(:count, :bigint, null: false, default: 0)
      add(:source, :text, null: false, default: "brain_cells")

      timestamps(type: :naive_datetime_usec)
    end

    create(unique_index(:symbrella_vocab_tokens, [:token]))
    create(unique_index(:symbrella_vocab_tokens, [:token_id]))
    create(index(:symbrella_vocab_tokens, [:source]))

    create(constraint(:symbrella_vocab_tokens, :token_id_nonnegative, check: "token_id >= 0"))

    create(constraint(:symbrella_vocab_tokens, :count_nonnegative, check: "count >= 0"))
  end
end
