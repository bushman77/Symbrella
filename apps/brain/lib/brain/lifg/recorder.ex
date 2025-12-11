defmodule Brain.LIFG.Recorder do
  @moduledoc """
  Side-effect helper to persist the *last* LIFG decision snapshot in the LIFG GenServer.

  - `maybe_record_last/6` safely sends a normalized payload to the server via
    `GenServer.cast(server, {:record_last, payload})` **iff** the server is running.
  - `build_last_payload/5` constructs a stable, compact structure suitable for UI/logs.
  - A deprecated `maybe_record_last/5` wrapper is provided for legacy call sites.

  This module is pure (no global state). The preferred API is to pass the **server module**
  (usually `Brain.LIFG` or `__MODULE__` from inside it) explicitly.
  """

  alias Brain.Utils.Safe

  @type payload :: %{
          ts_ms: non_neg_integer(),
          source: atom() | String.t(),
          si_sentence: any(),
          intent: any(),
          confidence: any(),
          tokens: list(),
          choices: list(),
          finalists: list(),
          guards: map(),
          feature_mix: map(),
          audit: map(),
          meta: map()
        }

  # ------------------------- Public API ---------------------------------------

  @doc """
  If `server` is running, record the latest decision payload via `{:record_last, payload}`.

  `server` may be:
    * a registered GenServer name (e.g., `Brain.LIFG`)
    * a PID of the running server

  Returns `:ok` regardless of server availability (best-effort, no-throw).
  """
  @spec maybe_record_last(atom() | pid(), atom() | String.t(), map(), list(), map(), map()) :: :ok
  def maybe_record_last(server, source, si, choices, audit, meta)
      when (is_atom(server) or is_pid(server)) and is_map(si) and is_list(choices) do
    pid =
      cond do
        is_pid(server) -> server
        is_atom(server) -> GenServer.whereis(server)
        true -> nil
      end

    if is_pid(pid) do
      payload = build_last_payload(source, si, choices, audit, meta)
      GenServer.cast(pid, {:record_last, payload})
    end

    :ok
  rescue
    _ -> :ok
  end

  @doc """
  DEPRECATED: use `maybe_record_last/6` and pass the server module explicitly.

  This keeps older call sites working:
    `maybe_record_last(:stage1, si, choices, audit, meta)`
  will forward to:
    `maybe_record_last(Brain.LIFG, :stage1, si, choices, audit, meta)`.
  """
  @deprecated "Pass the server module explicitly (e.g., __MODULE__). Use maybe_record_last/6."
  @spec maybe_record_last(atom() | String.t(), map(), list(), map(), map()) :: :ok
  def maybe_record_last(source, si, choices, audit, meta)
      when is_map(si) and is_list(choices) do
    maybe_record_last(Brain.LIFG, source, si, choices, audit, meta)
  end

  @doc """
  Build a compact, UI-friendly snapshot of the most recent decision.

  Fields:
    * `:tokens` — minimal token info (index/phrase/mw/n/span)
    * `:choices` — winner + scores per token (normalized), margin, alt_ids, slate_alt_ids
        - `:alt_ids` are scored competitors (derived upstream; Option A keeps slate-only out)
        - `:slate_alt_ids` are slate-only candidates (Option A), separate from scored competitors
    * `:finalists` — score ranking per token
    * `:guards` — boundary/chargram guard counters
    * `:feature_mix` — Stage-1 weights used
    * `:audit` — passthrough Stage-1 audit (if any)
    * `:meta` — caller-provided metadata (e.g., cover count, flips, pmtg mode)
  """
  @spec build_last_payload(atom() | String.t(), map(), list(), map(), map()) :: payload
  def build_last_payload(source, si, choices, audit, meta)
      when is_map(si) and is_list(choices) do
    now = System.system_time(:millisecond)

    tokens =
      Safe.get(si, :tokens, [])
      |> Enum.map(fn t ->
        %{
          index:
            Safe.get(t, :index) ||
              Safe.get(t, "index") ||
              Safe.get(t, :i) || 0,
          phrase:
            Safe.get(t, :phrase) ||
              Safe.get(t, "phrase") ||
              Safe.get(t, :text) ||
              Safe.get(t, :lemma),
          mw: Safe.get(t, :mw, false),
          n: Safe.get(t, :n, 1),
          span: Safe.get(t, :span)
        }
      end)

    finalists =
      Enum.map(choices, fn ch ->
        scores = Safe.get(ch, :scores, %{}) || %{}
        ranking = scores |> Enum.sort_by(fn {_id, s} -> -s end)

        %{
          token_index: Safe.get(ch, :token_index, 0),
          ranking: ranking
        }
      end)

    guards = %{
      chargram_violation:
        (is_map(audit) && Map.get(audit, :chargram_violation)) ||
          Safe.get(si, :chargram_violation, 0) || 0,
      rejected_by_boundary: (is_map(audit) && Map.get(audit, :rejected_by_boundary)) || [],
      missing_candidates: (is_map(audit) && Map.get(audit, :missing_candidates)) || 0,
      missing_candidate_tokens:
        (is_map(audit) && Map.get(audit, :missing_candidate_tokens)) || []
    }

    %{
      ts_ms: now,
      source: source,
      si_sentence: Safe.get(si, :sentence),
      intent: Safe.get(si, :intent),
      confidence: Safe.get(si, :confidence),
      tokens: tokens,
      choices:
        Enum.map(choices, fn ch ->
          alt_ids =
            (Safe.get(ch, :alt_ids, []) || [])
            |> Enum.reject(&is_nil/1)
            |> Enum.map(&to_string/1)

          slate_alt_ids =
            (Safe.get(ch, :slate_alt_ids, []) || [])
            |> Enum.reject(&is_nil/1)
            |> Enum.map(&to_string/1)

          %{
            token_index: Safe.get(ch, :token_index, 0),
            chosen_id: Safe.get(ch, :chosen_id),
            margin: Safe.get(ch, :margin, 0.0),
            scores: Safe.get(ch, :scores, %{}) || %{},
            alt_ids: alt_ids,
            slate_alt_ids: slate_alt_ids
          }
        end),
      finalists: finalists,
      guards: guards,
      feature_mix: Map.get(audit || %{}, :feature_mix, %{}),
      audit: audit || %{},
      meta: meta || %{}
    }
  end
end

