defmodule Core.Token do
  @moduledoc """
  Token struct + C64-mode subpipeline scaffold.

  `tokenize/1` does:
    1) normalize sentence
    2) greedy segmentation (phrases before words)
    3) short-term check: active BrainCells (all instances)
    4) long-term check: DB existence -> fetch all variants (all instances)
    5) remote POS (batched, last resort) for unknowns
    6) negative-cache unknowns that truly miss
    7) start/register all instances (no disambiguation yet)
    8) return a SemanticInput with `token_structs` (one token per instance)

  NOTE: This is a scaffold. Many calls referenced here are not implemented yet.
  It is expected to crash at runtime until those boundaries exist.
  """

  defstruct [
    :phrase,
    :span,            # {start, stop}
    :pos,
    :id,
    :variant,
    :source,          # :active | :db | :remote | :fallback
    :confidence,
    :definition,
    :example,
    :gram_function,
    synonyms: [],
    antonyms: [],
    semantic_atoms: []
  ]

  @type t :: %__MODULE__{
          phrase: String.t(),
          span: {non_neg_integer(), non_neg_integer()},
          pos: atom() | String.t() | nil,
          id: String.t() | nil,
          variant: non_neg_integer() | nil,
          source: :active | :db | :remote | :fallback | nil,
          confidence: float() | nil,
          definition: String.t() | nil,
          example: String.t() | nil,
          synonyms: [String.t()],
          antonyms: [String.t()],
          gram_function: String.t() | nil,
          semantic_atoms: [String.t()]
        }

  alias Core.SemanticInput, as: SI
  alias Core.Token, as: Tok

  @spec tokenize(String.t()) :: SI.t()
  def tokenize(sentence) when is_binary(sentence) do
    # 1) Normalize once (single source of truth for keys)
    norm = Core.Text.normalize(sentence)

    # 2) Greedy segmentation (DB/lexicon/heuristics/remote-phrase can be used inside)
    segs = Core.Segmenter.segment_phrases(norm)
    segs = List.wrap(segs)

    # Unique candidate phrases (normalized) from segments
    phrases =
      segs
      |> Enum.map(& &1.text)
      |> Enum.uniq()

    # 3) Short-term memory: all active instances already running for these phrases
    active_instances =
      phrases
      |> Enum.flat_map(fn phrase ->
        # Expect an index to list all IDs for a phrase (e.g., "kick the bucket|VERB|0", etc.)
        ids = Core.Brain.Index.ids_for_phrase(phrase)
        Enum.map(ids, fn id ->
          pid = Core.Brain.whereis(id)
          %{phrase: phrase, id: id, source: :active, pid: pid}
        end)
      end)

    # 4) Long-term memory (DB) — first a lightweight existence check, then fetch *all* variants
    db_instances =
      phrases
      |> Enum.reject(&NegCache.member?/1)
      |> Enum.flat_map(fn phrase ->
        if Db.phrase_exists?(phrase) do
          # Expect rows like: %{id: id, pos: pos, variant: int, definition: ..., example: ..., synonyms: [...], antonyms: [...], gram_function: ..., semantic_atoms: [...]}
          rows = Db.fetch_variants(phrase)
          Enum.map(rows, fn row -> Map.put(row, :source, :db) end)
        else
          []
        end
      end)

    # 5) Remote POS (batched, last resort) for phrases still unknown and not neg-cached
    unknowns_for_remote =
      phrases
      |> Enum.reject(&NegCache.member?/1)
      |> Enum.reject(&Db.phrase_exists?/1)

    remote_hits = RemotePOS.batch(unknowns_for_remote) # %{phrase => %{pos: POS | [POS], confidence: float, definition: ..., example: ..., synonyms: [...], antonyms: [...], gram_function: ..., semantic_atoms: [...]}}

    # Negative-cache true misses (unknowns that didn't come back as hits)
    _ = (unknowns_for_remote -- Map.keys(remote_hits)) |> Enum.each(&NegCache.put/1)

    remote_instances =
      remote_hits
      |> Enum.flat_map(fn {phrase, info} ->
        poss = case info[:pos] do
          list when is_list(list) -> list
          single -> [single]
        end

        Enum.map(poss, fn pos ->
          base = Map.drop(info, [:pos])
          Map.merge(base, %{
            phrase: phrase,
            pos: pos,
            id: "#{phrase}|#{pos}|0",
            variant: 0,
            source: :remote
          })
        end)
      end)

    # 6) Activate all instances (no disambiguation)
    instances = active_instances ++ db_instances ++ remote_instances

    _ =
      instances
      |> Enum.each(fn inst ->
        id = inst[:id] || "#{inst.phrase}|#{inst[:pos] || :UNK}|0"
        case Brain.get_or_start(id) do
          {:ok, pid} -> Brain.register_active(id, pid)
          {:error, {:already_started, pid}} -> Brain.register_active(id, pid)
          _ -> :ok
        end
      end)

    # 7) Build tokens (one token per instance); map spans from segments (first match)
    tokens =
      instances
      |> Enum.map(fn inst ->
        seg =
          Enum.find(segs, fn s ->
            Core.Text.normalize(s.text) == Core.Text.normalize(inst.phrase)
          end) || %{start: 0, stop: 0}

        %Tok{
          phrase: inst.phrase,
          span: {seg.start, seg.stop},
          pos: inst[:pos],
          id: inst[:id],
          variant: inst[:variant],
          source: inst[:source],
          confidence: inst[:confidence],
          definition: inst[:definition],
          example: inst[:example],
          synonyms: inst[:synonyms] || [],
          antonyms: inst[:antonyms] || [],
          gram_function: inst[:gram_function],
          semantic_atoms: inst[:semantic_atoms] || []
        }
      end)

    # 8) Return the SemanticInput ready for downstream stages
    %SI{
      sentence: norm,
      original_sentence: sentence,
      source: :console,
      tokens: tokens
    }
  end
end

