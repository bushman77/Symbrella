defmodule Brain.LIFG.Prior do
  @moduledoc """
  Static prior weights for lexicon senses before LIFG Stage-1.

  This module computes a **pre-context prior** for a BrainCell-like record
  (map or struct). The intent is:

    * All senses for a norm are loaded.
    * `status` and `semantic_atoms` shape a *bias*, not a hard gate.
    * Closed-class senses (pronouns, determiners) get special handling so
      everyday readings dominate, but marked variants remain available.

  The output of `prior_for_cell/1` is typically fed into Stage-1 as the
  `:rel_prior` feature for each candidate.

  ### Inputs

  `prior_for_cell/1` accepts any map/struct that looks like a `Db.BrainCell`,
  i.e. responds to (either atom or string keys):

    * `:word`               – surface form
    * `:pos`                – coarse POS like \"noun\", \"verb\", \"pronoun\"
    * `:semantic_atoms`     – list of strings such as \"pos_raw:pron\",
                               \"tag:historical\", \"tag:colloquial\", ...
    * `:status`             – \"active\" or anything else

  ### Output

  A float in `[0.0, 1.0]` representing how plausible this sense is *before*
  any contextual evidence (lexical fit, intent, activation, episodes).

  The general scheme is:

    * classify the sense into a bucket:
        - `:core_everyday`
        - `:marked_variant`
        - `:marginal`
        - `:exotic`
    * detect whether it is **closed-class** (pronoun/determiner)
    * map `(closed_class?, bucket)` to a base prior
    * nudge slightly based on `status` (\"active\" vs other)
    * clamp the result into `[0.0, 1.0]`

  ## Examples

  Everyday pronoun vs name-like exotic sense:

      iex> alias Brain.LIFG.Prior
      iex> pron = %{
      ...>   word: "you",
      ...>   pos: "pronoun",
      ...>   semantic_atoms: ["ety:1", "pos_raw:pron"],
      ...>   status: "inactive"
      ...> }
      iex> exotic = %{
      ...>   word: "you",
      ...>   pos: "phrase",
      ...>   semantic_atoms: ["ety:2", "tag:historical", "pos_raw:name"],
      ...>   status: "inactive"
      ...> }
      iex> p_pron   = Prior.prior_for_cell(pron)
      iex> p_exotic = Prior.prior_for_cell(exotic)
      iex> p_pron > 0.9
      true
      iex> p_exotic < p_pron
      true

  Closed-class everyday pronoun should outrank an open-class letter-name noun:

      iex> noun = %{
      ...>   word: "you",
      ...>   pos: "noun",
      ...>   semantic_atoms: ["ety:2", "pos_raw:noun"],
      ...>   status: "inactive"
      ...> }
      iex> Prior.prior_for_cell(pron) > Prior.prior_for_cell(noun)
      true
  """

  alias Brain.Utils.Safe

  @type cell_like :: map() | struct()

  @type bucket ::
          :core_everyday
          | :marked_variant
          | :marginal
          | :exotic

  @doc """
  Compute a static prior for a lexicon sense (BrainCell-like record).

  The prior is a float in `[0.0, 1.0]` and is intended to serve as the
  `:rel_prior` feature for LIFG Stage-1.

  See the moduledoc for the classification rules.
  """
  @spec prior_for_cell(cell_like()) :: float()
  def prior_for_cell(cell) do
    m = Safe.to_plain(cell)

    pos_raw = pos_raw(m)
    tags = tags(m)
    closed? = closed_class?(pos_raw, m)
    bucket = bucket_for(pos_raw, tags, closed?)

    base = base_prior(bucket, closed?)

    status =
      Map.get(m, :status) ||
        Map.get(m, "status") ||
        "inactive"

    base
    |> tweak_for_status(status)
    |> clamp01()
  end

  # ───────────────────────────── Internal classification ─────────────────────────────

  # Extract "pos_raw:*" from semantic_atoms when present, otherwise fall back
  # to `:pos` as a coarse hint.
  @spec pos_raw(map()) :: String.t() | nil
  defp pos_raw(m) do
    atoms =
      Map.get(m, :semantic_atoms) ||
        Map.get(m, "semantic_atoms") ||
        []

    atoms =
      atoms
      |> Safe.ensure_list()
      |> Enum.map(&Safe.to_plain/1)

    case Enum.find(atoms, &pos_raw_atom?/1) do
      "pos_raw:" <> rest ->
        rest |> String.downcase() |> String.trim()

      _ ->
        coarse_pos =
          Map.get(m, :pos) ||
            Map.get(m, "pos")

        coarse_pos
        |> to_string_if_present()
        |> case do
          nil -> nil
          s -> s |> String.downcase() |> String.trim()
        end
    end
  end

  defp pos_raw_atom?(s) when is_binary(s), do: String.starts_with?(s, "pos_raw:")
  defp pos_raw_atom?(_), do: false

  @spec tags(map()) :: [String.t()]
  defp tags(m) do
    atoms =
      Map.get(m, :semantic_atoms) ||
        Map.get(m, "semantic_atoms") ||
        []

    atoms
    |> Safe.ensure_list()
    |> Enum.map(&Safe.to_plain/1)
    |> Enum.reduce([], fn
      "tag:" <> rest, acc when is_binary(rest) ->
        [String.downcase(String.trim(rest)) | acc]

      _, acc ->
        acc
    end)
  end

  @spec closed_class?(String.t() | nil, map()) :: boolean()
  defp closed_class?(nil, m) do
    # Fallback: look at coarse POS if pos_raw is missing
    coarse_pos =
      Map.get(m, :pos) ||
        Map.get(m, "pos")

    coarse_pos =
      coarse_pos
      |> to_string_if_present()
      |> case do
        nil -> ""
        s -> s |> String.downcase() |> String.trim()
      end

    closed_pos?(coarse_pos)
  end

  defp closed_class?(pos_raw, _m) when is_binary(pos_raw) do
    closed_pos?(String.downcase(String.trim(pos_raw)))
  end

  defp closed_pos?(pos) do
    # keep this list small and conservative; we can expand later if needed
    pos in ["pron", "pronoun", "det", "determiner"]
  end

  @spec bucket_for(String.t() | nil, [String.t()], boolean()) :: bucket()
  defp bucket_for(pos_raw, tags, closed?) do
    pos_raw = pos_raw || ""

    name_like? =
      pos_raw in ["name"] or
        "name" in tags

    historical? = "historical" in tags or "archaic" in tags
    honourific? = "honorific" in tags
    colloquial? = "colloquial" in tags or "slang" in tags or "us" in tags

    cond do
      name_like? ->
        :exotic

      historical? and not closed? ->
        :exotic

      historical? and closed? ->
        :marginal

      honourific? ->
        # honourific pronouns (e.g. deity \"You\") are special but not core
        if closed?, do: :marked_variant, else: :marginal

      colloquial? ->
        :marked_variant

      true ->
        # default: treat as everyday unless other signals say otherwise
        :core_everyday
    end
  end

  @spec base_prior(bucket(), boolean()) :: float()
  defp base_prior(bucket, closed?) do
    case {closed?, bucket} do
      # Closed-class (pronoun/determiner) — strong priors
      {true, :core_everyday} -> 1.0
      {true, :marked_variant} -> 0.7
      {true, :marginal} -> 0.4
      {true, :exotic} -> 0.2
      # Open-class defaults
      {false, :core_everyday} -> 0.8
      {false, :marked_variant} -> 0.5
      {false, :marginal} -> 0.3
      {false, :exotic} -> 0.1
    end
  end

  @spec tweak_for_status(float(), String.t()) :: float()
  defp tweak_for_status(base, status) when is_binary(status) do
    norm_status = status |> String.downcase() |> String.trim()

    # `status` is a nudge, not a gate:
    #   * \"active\" → slight boost
    #   * anything else → slight dampening
    case norm_status do
      "active" ->
        base * 1.25

      _ ->
        base * 0.9
    end
  end

  defp tweak_for_status(base, _), do: base

  # ───────────────────────────── Small utilities ─────────────────────────────

  defp clamp01(n) when is_number(n) do
    n
    |> max(0.0)
    |> min(1.0)
  end

  defp clamp01(_), do: 0.0

  defp to_string_if_present(nil), do: nil
  defp to_string_if_present(v) when is_binary(v), do: v
  defp to_string_if_present(v), do: to_string(v)
end
