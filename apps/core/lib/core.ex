defmodule Core do
  @moduledoc """
  Core pipeline: tokenization → STM → LTM → lexicon → LIFG Stage-1.
  """

  alias Core.{Lexicon, Token, Vectors}
  alias Brain, as: CBrain

  @type si :: map()
  @type lifg_opts :: keyword()

  @doc """
  Resolve a phrase and run LIFG Stage-1 (side effects: boosts/inhibitions are applied).
  Attaches `:lifg_choices` and an audit crumb in `:trace`.
  """
  @spec resolve_input(String.t(), lifg_opts()) :: si()
  def resolve_input(phrase, lifg_opts \\ []) when is_binary(phrase) do
    si =
      phrase
      |> Token.tokenize()
      |> CBrain.stm()
      |> Db.ltm()
      |> Lexicon.all()
      |> Brain.LIFG.disambiguate_stage1() 


    cand_map = build_lifg_candidates_by_token(si)
    ctx      = context_vec(si)

    defaults = [
      normalize: :softmax,
      scores: :top2,
      margin_threshold: 0.12,
      parallel: :auto,
      vector_lookup: &Vectors.fetch/1,        # <- NEW: vectors on demand
      apply_signals?: true,                   # you can set false for dry-runs
      delta_model: :margin_scaled             # <- NEW: gentle scaling (see Brain wiring)
    ]

    {:ok, lifg} =
      CBrain.lifg_stage1(%{candidates_by_token: cand_map}, ctx, Keyword.merge(defaults, lifg_opts))

    si
    |> Map.put(:lifg_choices, lifg.choices)
    |> Map.update(:trace, [], &[{:lifg_stage1, lifg.audit} | &1])
  end

  @doc "Read a cell’s status (id is a string)."
  def cell_status(id), do: GenServer.call(CBrain, {:cell, id, :status})

  @doc "Send any cast message to a cell (e.g., :activate, :stop)."
  def cell_cast(id, msg), do: GenServer.cast(CBrain, {:cell, id, msg})

  @doc """
  Optional: activate a batch of cells from `si` (side effect); returns `si`.
  Tip: Keep this *small* to avoid background noise; LIFG will do the heavy lifting.
  """
  def activate_cells(si, opts \\ [delta: 0.1]) do                 # was 1 → gentler default
    payload = Map.new(opts)
    GenServer.cast(CBrain, {:activate_cells, si, payload})
    si
  end

  # ───────────────────────── LIFG helpers ─────────────────────────

  @doc false
  defp build_lifg_candidates_by_token(%{tokens: toks} = si) when is_list(toks) do
    rows = Map.get(si, :db_cells, []) || []

    toks
    |> Enum.with_index()
    |> Map.new(fn {tok, tidx} ->
      lemma = String.downcase(tok.phrase || "")

      senses =
        rows
        # keep only those matching this lemma by norm
        |> Enum.filter(&match_norm?(&1, lemma))
        # dedupe by canonical (norm,pos,sense) to avoid "Hello|..." vs "hello|..." dupes
        |> Enum.uniq_by(&canonical_key/1)
        |> Enum.map(fn row ->
          %{
            id: row.id,                           # real cell id for signals
            pos: row.pos || "noun",
            token_index: tidx,
            lemma: lemma,
            embedding: nil,                       # <- prefer vector_lookup path
            embedding_id: {row.norm, row.pos},    # <- key for Core.Vectors.fetch/1
            lex_fit: 0.6,                         # stub; replace when ready
            rel_prior: 0.5,
            intent_bias: 0.0,
            activation: row.activation || 0.0
          }
        end)

      {tidx, senses}
    end)
    |> Enum.reject(fn {_i, senses} -> senses == [] end)
    |> Map.new()
  end

  defp build_lifg_candidates_by_token(_), do: %{}

  defp match_norm?(%{norm: norm}, lemma) when is_binary(norm), do: String.downcase(norm) == lemma
  defp match_norm?(_, _), do: false

  defp canonical_key(%{norm: n, pos: p, id: id}) do
    s =
      case String.split(to_string(id), "|") do
        [_w, _p, sense] -> sense
        _ -> "0"
      end

    {String.downcase(n || ""), String.downcase(p || ""), s}
  end

  @doc false
  defp context_vec(%{tokens: toks}) when is_list(toks) and toks != [] do
    # Simple pooled context: average of lemma vectors
    toks
    |> Enum.map(&String.downcase(&1.phrase || ""))
    |> Enum.map(&Vectors.fetch/1)
    |> Enum.reject(&is_nil/1)
    |> average_vec() || [1.0, 0.0, 0.0]
  end
  defp context_vec(_), do: [1.0, 0.0, 0.0]

  defp average_vec(list) when is_list(list) and list != [] do
    dim = list |> hd() |> length()
    sum =
      list
      |> Enum.reduce(List.duplicate(0.0, dim), fn v, acc ->
        Enum.zip_with(acc, v, &(&1 + &2))
      end)

    n = length(list) |> max(1)
    Enum.map(sum, &(&1 / n))
  end
  defp average_vec(_), do: nil
end

