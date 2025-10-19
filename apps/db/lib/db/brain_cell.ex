defmodule Db.BrainCell do
  @moduledoc """
  Persistent representation of a Brain Cell.

  This schema **enforces strict invariants** to prevent placeholder/unknown cells
  from entering storage. It aligns with DB CHECK constraints and Brain.Cell runtime guards.

  ## Invariants
  - `id` shape: `"text|pos"` or `"text|pos|suffix"`; no trailing pipe.
  - `pos` ∈ allowed POS; never `"unk"`.
  - `id`'s POS segment must equal `pos` column.
  - `id` must not contain `|unk|` or `|seed|`.
  - `norm` is the normalized `word` (lowercase, trimmed, single-spaced).

  Notes:
  - `gram_function` is an array (`text[]`) with POS-aware allowed values only.
  """

  use Ecto.Schema
  import Ecto.Changeset

  @type t :: %__MODULE__{}

  @primary_key {:id, :string, autogenerate: false}
  @timestamps_opts [type: :naive_datetime]

  @derive {Jason.Encoder,
           only: [
             :id,
             :word,
             :norm,
             :pos,
             :type,
             :gram_function,
             :definition,
             :example,
             :synonyms,
             :antonyms,
             :semantic_atoms,
             :status,
             :activation,
             :modulated_activation,
             :dopamine,
             :serotonin,
             :position,
             :connections,
             :last_dose_at,
             :last_substance,
             :token_id
           ]}

  @allowed_pos ~w(
    noun verb adjective adverb interjection phrase proper_noun pronoun
    determiner preposition conjunction numeral particle
  )

  # Allows third segment to be alnum/underscore (index or tag like "fallback")
  @id_regex ~r/^[^|]+?\|(noun|verb|adjective|adverb|interjection|phrase|proper_noun|pronoun|determiner|preposition|conjunction|numeral|particle)(\|[A-Za-z0-9_]+)?$/

  # POS-aware grammar allowlist (matches the DB CHECK)
  @gf_allow %{
    "noun" => ~w(countable uncountable plural-only usually plural),
    "verb" => ~w(transitive intransitive ditransitive ambitransitive copular auxiliary modal ergative impersonal),
    "adjective" => ~w(attributive-only predicative-only postpositive comparative-only)
  }

  # common “spelling” normalizations we accept on input and canonicalize
  @gf_aliases %{
    "plural only" => "plural-only",
    "comparative only" => "comparative-only",
    "predicative only" => "predicative-only",
    "attributive only" => "attributive-only"
  }

  schema "brain_cells" do
    field :word, :string
    field :norm, :string
    field :pos, :string
    field :definition, :string
    field :example, :string

    # ✅ array type to match Postgres text[]
    field :gram_function, {:array, :string}, default: []

    field :synonyms, {:array, :string}, default: []
    field :antonyms, {:array, :string}, default: []
    field :semantic_atoms, {:array, :string}, default: []

    field :type, :string
    field :status, :string, default: "inactive"

    field :activation, :float, default: 0.0
    field :modulated_activation, :float, default: 0.0
    field :dopamine, :float, default: 0.0
    field :serotonin, :float, default: 0.0

    field :position, {:array, :float}
    field :connections, {:array, :map}, default: []
    field :last_dose_at, :utc_datetime_usec
    field :last_substance, :string
    field :token_id, :integer
    # field :embedding, Pgvector.Ecto.Vector

    timestamps()
  end

  @doc """
  Strict changeset for inserts/updates.

  - Normalizes `word` and sets `norm` from it if not provided.
  - Trims `id` (and strips trailing pipes) before validating.
  - Enforces POS whitelist and `id` shape/consistency.
  - Bans `|unk|`/`|seed|` anywhere in `id` and `pos == "unk"`.
  - Sanitizes arrays (synonyms/antonyms/semantic_atoms/gram_function).
  """
  @spec changeset(t(), map()) :: Ecto.Changeset.t()
  def changeset(cell, attrs) do
    cell
    |> cast(attrs, __schema__(:fields))
    |> validate_required([:id, :word, :pos])
    |> normalize_text_fields()
    |> ensure_norm_matches_word()
    |> sanitize_arrays()
    |> validate_pos_whitelist()
    |> validate_id_not_placeholder()
    |> validate_change(:id, &validate_id_shape/2)
    |> validate_id_pos_matches_column()
  end

  # ───────────────────────── helpers ─────────────────────────

  defp normalize_text_fields(changeset) do
    changeset
    |> update_change(:word, &normalize_word/1)
    |> update_change(:norm, &normalize_word/1)
    |> update_change(:pos, &normalize_pos/1)
    |> update_change(:type, &trim_or_nil/1)
    |> update_change(:status, &trim_or_nil/1)
    # id: trim & drop trailing pipes, but keep internal spaces (phrases allowed)
    |> update_change(:id, fn
      nil -> nil
      s when is_binary(s) ->
        s
        |> String.trim()
        |> String.replace(~r/\|+$/u, "")
      other -> other
    end)
  end

  defp ensure_norm_matches_word(%Ecto.Changeset{changes: %{word: w}} = cs) when is_binary(w) do
    put_change(cs, :norm, normalize_word(w))
  end

  defp ensure_norm_matches_word(%Ecto.Changeset{} = cs), do: cs

  defp sanitize_arrays(%Ecto.Changeset{} = cs) do
    pos = get_field(cs, :pos)

    cs
    |> update_change(:synonyms, &sanitize_string_array/1)
    |> update_change(:antonyms, &sanitize_string_array/1)
    |> update_change(:semantic_atoms, &sanitize_string_array/1)
    |> update_change(:gram_function, &sanitize_gf_array(&1, pos))
  end

  defp sanitize_string_array(list) when is_list(list) do
    list
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp sanitize_string_array(_), do: []

  # POS-aware cleanup + clamp to allowlist. Also synthesizes "usually plural"
  # for nouns when both "usually" and "plural" are present as separate tags.
  defp sanitize_gf_array(list, pos) when is_list(list) do
    raw =
      list
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&normalize_tag/1)

    tags =
      case pos do
        "noun" ->
          synth =
            if Enum.member?(raw, "usually") and Enum.member?(raw, "plural"),
              do: ["usually plural"],
              else: []
          keep(raw ++ synth, @gf_allow["noun"])

        "verb" ->
          keep(raw, @gf_allow["verb"])

        "adjective" ->
          keep(raw, @gf_allow["adjective"])

        _ ->
          # other POS: we don't restrict, but still dedup and trim
          Enum.uniq(raw)
      end

    tags
  end

  defp sanitize_gf_array(_other, _pos), do: []

  defp keep(tags, allow) do
    tags
    |> Enum.map(&canonicalize/1)
    |> Enum.filter(&(&1 in allow))
    |> Enum.uniq()
  end

  defp canonicalize(s), do: Map.get(@gf_aliases, s, s)

  defp normalize_tag(t) when is_binary(t) do
    t
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize_tag(t), do: t |> to_string() |> normalize_tag()

  defp validate_pos_whitelist(%Ecto.Changeset{} = cs) do
    validate_inclusion(cs, :pos, @allowed_pos, message: "must be a known POS")
  end

  defp validate_id_not_placeholder(%Ecto.Changeset{} = cs) do
    cs
    |> validate_change(:id, fn
      :id, id when is_binary(id) ->
        if String.contains?(id, "|unk|") or String.contains?(id, "|seed|") do
          [id: "must not contain placeholder segments (|unk| or |seed|)"]
        else
          []
        end

      _k, _v ->
        []
    end)
    |> validate_change(:pos, fn
      :pos, "unk" -> [pos: "must not be 'unk'"]
      _k, _v -> []
    end)
  end

  # ——— validate_change callback: (field, value) -> keyword ———
  defp validate_id_shape(:id, id) when is_binary(id) do
    if Regex.match?(@id_regex, id) do
      []
    else
      [id: "has invalid shape; expected text|pos or text|pos|suffix (alnum/_), no trailing pipe"]
    end
  end

  defp validate_id_shape(_k, _v), do: []

  # ——— needs access to :pos column, so we capture it from the changeset ———
  defp validate_id_pos_matches_column(%Ecto.Changeset{} = cs) do
    pos = get_field(cs, :pos)

    validate_change(cs, :id, fn
      :id, id when is_binary(id) ->
        cond do
          not is_binary(pos) ->
            [id: "cannot verify POS without :pos"]

          extract_pos(id) != pos ->
            [id: "POS in id (#{extract_pos(id) || "nil"}) must match column :pos (#{pos})"]

          true ->
            []
        end

      _k, _v ->
        []
    end)
  end

  # --- primitive normalizers ------------------------------------------------

  defp normalize_word(nil), do: nil

  defp normalize_word(s) when is_binary(s) do
    s
    |> String.downcase()
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
  end

  defp normalize_word(other), do: other

  defp normalize_pos(nil), do: nil

  defp normalize_pos(s) when is_binary(s) do
    s
    |> String.trim()
    |> String.downcase()
  end

  defp normalize_pos(other), do: other

  defp trim_or_nil(nil), do: nil

  defp trim_or_nil(s) when is_binary(s) do
    s = String.trim(s)
    if s == "", do: nil, else: s
  end

  defp trim_or_nil(other), do: other

  defp extract_pos(id) when is_binary(id) do
    id
    |> String.split("|", trim: true)
    |> Enum.at(1)
  end

  defp extract_pos(_), do: nil
end

