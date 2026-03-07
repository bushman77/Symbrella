# apps/brain/lib/brain/hippocampus/writer.ex
defmodule Brain.Hippocampus.Writer do
  @moduledoc """
  Hippocampus episode persistence.

  IMPORTANT:
  - brain must not depend on core (db <- brain <- core <- web).
  - Therefore, anything we persist must be JSON-safe without requiring Core structs
    (e.g., `%Core.Token{}`) to implement Jason.Encoder.

  This module deep-normalizes incoming SI/episode payloads into plain maps/lists
  before JSON encoding, preventing Jason.Encoder crashes.
  """

  require Logger

  alias Brain.Utils.Safe
  alias Db

  @episodes_table "episodes"

  # ---------------------------------------------------------------------------
  # Public
  # ---------------------------------------------------------------------------

  @doc """
  Persist an episode if enabled via opts.

  Expects:
    - `episode` map-like (may contain structs)
    - `opts` keyword with `:persist?` boolean

  Returns:
    - :ok (best-effort; never raises to callers)
  """
  @spec maybe_persist(map(), keyword()) :: :ok
  def maybe_persist(%{} = episode, opts) when is_list(opts) do
    if Keyword.get(opts, :persist?, true) do
      try do
        insert_row!(episode)
        :ok
      rescue
        e ->
          Logger.debug("Hippo.Writer failed: #{Exception.message(e)}")
          :ok
      catch
        :exit, reason ->
          Logger.debug("Hippo.Writer exit: #{inspect(reason)}")
          :ok

        kind, reason ->
          Logger.debug("Hippo.Writer throw/error: #{inspect({kind, reason})}")
          :ok
      end
    else
      :ok
    end
  end

  def maybe_persist(_other, _opts), do: :ok

  @doc """
  Insert an episode row (raises on DB errors).

  This is called internally by `maybe_persist/2` and is allowed to raise;
  callers are expected to wrap it.
  """
  @spec insert_row!(map()) :: :ok
  def insert_row!(%{} = episode) do
    row = episode_row(episode)

Db.insert_all(@episodes_table, [row],
  on_conflict: :nothing,
  conflict_target: [:signature]
)

    :ok
  end

  # ---------------------------------------------------------------------------
  # Row builder
  # ---------------------------------------------------------------------------

  defp episode_row(%{} = episode) do
    ep = deep_plain(episode)

    at_ms = now_ms()
    signature = build_signature(ep)

    si = Safe.get(ep, :si) || Safe.get(ep, "si") || %{}
    cues = Safe.get(ep, :cues) || Safe.get(ep, "cues") || []
    winners = Safe.get(ep, :winners) || Safe.get(ep, "winners") || []
    emotion = Safe.get(ep, :emotion) || Safe.get(ep, "emotion") || %{}
    meta = Safe.get(ep, :meta) || Safe.get(ep, "meta") || %{}

    # Compact payloads *and* enforce JSON-safe shapes.
    si2 = compact_si(si)
    emotion2 = compact_emotion(emotion)
    winners2 = compact_winners(winners)

    %{
      signature: signature,
      at_ms: Safe.get(ep, :at_ms) || Safe.get(ep, "at_ms") || at_ms,
      cues: Jason.encode!(deep_plain(cues)),
      winners: Jason.encode!(deep_plain(winners2)),
      si: Jason.encode!(deep_plain(si2)),
      emotion: Jason.encode!(deep_plain(emotion2)),
      meta: Jason.encode!(deep_plain(meta)),
      inserted_at: DateTime.utc_now() |> DateTime.truncate(:second),
      updated_at: DateTime.utc_now() |> DateTime.truncate(:second)
    }
  end

  # ---------------------------------------------------------------------------
  # Compactors (JSON-safe)
  # ---------------------------------------------------------------------------

  # Keep only a bounded slice of SI. Must be JSON-safe.
  defp compact_si(si) do
    si0 = deep_plain(si)

    # Keep tokens minimal and JSON-safe. Tokens may be Core.Token structs.
    toks =
      si0
      |> Safe.get(:tokens, [])
      |> List.wrap()
      |> Enum.take(64)
      |> Enum.map(&compact_token/1)

    # Active cells can include Db.BrainCell structs. Make them plain and bounded.
    active =
      si0
      |> Safe.get(:active_cells, [])
      |> List.wrap()
      |> Enum.take(128)
      |> Enum.map(&compact_active_cell/1)

    trace =
      si0
      |> Safe.get(:trace, [])
      |> List.wrap()
      |> Enum.take(64)
      |> Enum.map(&deep_plain/1)

    %{
      sentence: Safe.get(si0, :sentence),
      intent: Safe.get(si0, :intent),
      keyword: Safe.get(si0, :keyword),
      confidence: Safe.get(si0, :confidence),
      lifg_choices: Safe.get(si0, :lifg_choices),
      lifg_opts: Safe.get(si0, :lifg_opts),
      tokens: toks,
      active_cells: active,
      trace: trace
    }
  end

  defp compact_token(tok) do
    t = deep_plain(tok)

    %{
      index: Safe.get(t, :index) || Safe.get(t, :token_index),
      token_index: Safe.get(t, :token_index) || Safe.get(t, :index),
      phrase: Safe.get(t, :phrase),
      norm: Safe.get(t, :norm),
      span: Safe.get(t, :span),
      mw: Safe.get(t, :mw),
      n: Safe.get(t, :n),
      kind: Safe.get(t, :kind),
      pos: Safe.get(t, :pos),
      subpos: Safe.get(t, :subpos),
      source: Safe.get(t, :source)
    }
  end

  defp compact_active_cell(cell) do
    c = deep_plain(cell)

    %{
      id: Safe.get(c, :id),
      norm: Safe.get(c, :norm),
      lemma: Safe.get(c, :lemma),
      pos: Safe.get(c, :pos),
      token_index: Safe.get(c, :token_index),
      score: Safe.get(c, :score),
      source: Safe.get(c, :source)
    }
  end

  defp compact_winners(winners) do
    winners
    |> List.wrap()
    |> Enum.take(32)
    |> Enum.map(fn w ->
      x = deep_plain(w)

      %{
        id: Safe.get(x, :id),
        norm: Safe.get(x, :norm),
        lemma: Safe.get(x, :lemma),
        token_index: Safe.get(x, :token_index),
        score: Safe.get(x, :score),
        margin: Safe.get(x, :margin),
        raw: Safe.get(x, :raw)
      }
    end)
  end

  defp compact_emotion(emotion) do
    e = deep_plain(emotion)

    # Keep only stable fields; avoid huge/volatile payloads.
    %{
      valence: Safe.get(e, :valence),
      arousal: Safe.get(e, :arousal),
      tone_reaction: Safe.get(e, :tone_reaction),
      latents: Safe.get(e, :latents),
      from: Safe.get(e, :from)
    }
  end

  # ---------------------------------------------------------------------------
  # Signature
  # ---------------------------------------------------------------------------

  defp build_signature(ep) do
    # Signature should be stable and not include structs.
    cues =
      ep
      |> Safe.get(:cues, [])
      |> List.wrap()
      |> Enum.map(&to_string/1)
      |> Enum.map(&String.downcase/1)
      |> Enum.sort()
      |> Enum.take(16)

    win_ids =
      ep
      |> Safe.get(:winners, [])
      |> List.wrap()
      |> Enum.map(&deep_plain/1)
      |> Enum.map(fn w -> Safe.get(w, :id) || "" end)
      |> Enum.reject(&(&1 == ""))
      |> Enum.sort()
      |> Enum.take(16)

    base = Enum.join(cues, "|") <> "||" <> Enum.join(win_ids, "|")
    :crypto.hash(:sha256, base) |> Base.encode16(case: :lower)
  end

  # ---------------------------------------------------------------------------
  # Deep normalization (JSON-safe)
  # ---------------------------------------------------------------------------

  # Convert any nested structs into JSON-safe maps recursively.
  defp deep_plain(%_{} = s) do
    s
    |> Map.from_struct()
    |> Map.drop([:__struct__, :__meta__])
    |> Enum.map(fn {k, v} -> {k, deep_plain(v)} end)
    |> Map.new()
  end

  defp deep_plain(%{} = m) do
    m
    |> Enum.map(fn {k, v} -> {k, deep_plain(v)} end)
    |> Map.new()
  end

  defp deep_plain(list) when is_list(list), do: Enum.map(list, &deep_plain/1)
  defp deep_plain(other), do: other

  defp now_ms, do: System.system_time(:millisecond)
end
