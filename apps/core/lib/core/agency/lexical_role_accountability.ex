defmodule Core.Agency.LexicalRoleAccountability do
  @moduledoc """
  Durable, compact accountability receipts for the production LIFG role audit.

  The live Core pipeline always emits the read-only LexicalRoleShadow trace.
  A production turn with a valid session ID additionally records that result
  in the existing agency ledger. Persistence failures are themselves visible
  in the trace but never interrupt comprehension, mutate lexical winners,
  or authorize a new strategy.

  The persisted receipt contains only bounded metadata and a diagnostic
  fingerprint of the selected LIFG slate, not user prose or dictionary IDs.
  A hash is not an authenticated proof of provenance.
  """

  alias Core.Agency.LexicalRoleShadow
  alias Core.Pipeline.Trace

  @source "lexical_role_accountability"
  @max_session_bytes 128
  @default_limit 20

  @spec record(map(), keyword()) :: map()
  def record(si, opts \\ [])

  def record(%{} = si, _opts) do
    case eligible?(si) do
      :ok ->
        with {:ok, attributes} <- attrs(si),
             {:ok, row} <- insert(attributes) do
          trace_record(si, :persisted, :agency_event_recorded, Map.get(row, :id))
        else
          {:error, :missing_role_audit} ->
            trace_record(si, :unavailable, :missing_role_audit)

          {:error, _reason} ->
            trace_record(si, :unavailable, :storage_unavailable)
        end

      {:skip, reason} ->
        trace_record(si, :skipped, reason)
    end
  end

  def record(other, _opts), do: other

  @doc "Build the persisted accountability payload without writing to the database."
  @spec attrs(map()) :: {:ok, map()} | {:error, atom()}
  def attrs(%{} = si) do
    case role_event(si) do
      %{meta: %{} = meta, decision: decision, reason: reason} ->
        final_fingerprint = LexicalRoleShadow.selected_choices_sha256(si)
        original_fingerprint = Map.get(meta, :selected_choices_sha256)

        {:ok,
         %{
           agency_v: 1,
           session_id: Map.get(si, :session_id),
           actor: "symbrella",
           source: @source,
           status: "observed",
           action: "audit_lexical_role",
           input: %{
             frame_run_id: Map.get(si, :frame_run_id),
             frame_seq: Map.get(si, :frame_seq),
             role_audit_ts_ms: Map.get(role_event(si), :ts_ms),
             token_count: token_count(si),
             original_lifg_sha256: original_fingerprint
           },
           decision: %{
             v: Map.get(meta, :v),
             status: decision,
             construction: Map.get(meta, :construction),
             blocked_role_count: Map.get(meta, :blocked_role_count, 0),
             blocked_roles: List.wrap(Map.get(meta, :blocked_roles, [])) |> Enum.take(7),
             alternatives: List.wrap(Map.get(meta, :roles, [])) |> Enum.take(7),
             unresolved_role_count: Map.get(meta, :unresolved_role_count, 0)
           },
           reasons: %{
             diagnostic_reason: reason,
             structural_evidence: :independently_revalidated,
             alternative_evidence: :scored_attested_role_witness_only
           },
           self_model: %{},
           self_state: %{},
           outcome: %{
             final_lifg_sha256: final_fingerprint,
             selected_choices_changed_after_audit?:
               is_binary(original_fingerprint) and
                 is_binary(final_fingerprint) and
                 original_fingerprint != final_fingerprint,
             formal_derivation_verified?:
               Map.get(meta, :formal_derivation_verified?, false),
             lexical_sense_verified?: false,
             learning_applied?: false,
             factual_authority?: false
           },
           reflection: %{
             v: 1,
             type: :observational_accountability,
             signals: [],
             next_time_adjustment: :none,
             outcome_verified?: false
           }
         }}

      _ ->
        {:error, :missing_role_audit}
    end
  end

  def attrs(_), do: {:error, :missing_role_audit}

  @doc "Retrieve audit receipts independently of behavioral agency memory."
  @spec recent(String.t(), keyword()) :: [map()]
  def recent(session_id, opts \\ [])
      when is_binary(session_id) and is_list(opts) do
    limit = Keyword.get(opts, :limit, @default_limit)

    if valid_session?(session_id) do
      try do
        Db.AgencyEvents.recent(
          session_id: session_id,
          limit: limit,
          include_sources: [@source]
        )
      rescue
        _ -> []
      catch
        _, _ -> []
      end
    else
      []
    end
  end

  def recent(_, _), do: []

  defp role_event(si) do
    si
    |> Map.get(:trace, [])
    |> List.wrap()
    |> Enum.find(fn
      %{stage: :reflective_role_shadow} -> true
      _ -> false
    end)
  end

  defp eligible?(%{source: :prod} = si) do
    cond do
      Application.get_env(:core, :agency_ledger_enabled?, true) != true ->
        {:skip, :agency_ledger_disabled}

      not valid_session?(Map.get(si, :session_id)) ->
        {:skip, :missing_session_scope}

      true ->
        :ok
    end
  end

  defp eligible?(_), do: {:skip, :not_production_input}

  defp valid_session?(value) do
    is_binary(value) and byte_size(value) in 1..@max_session_bytes and
      String.trim(value) != ""
  end

  defp token_count(si) do
    si
    |> Map.get(:tokens, [])
    |> List.wrap()
    |> length()
    |> min(256)
  end

  defp insert(attributes) do
    try do
      Db.AgencyEvents.create_event(attributes)
    rescue
      _ -> {:error, :database_unavailable}
    catch
      _, _ -> {:error, :database_unavailable}
    end
  end

  defp trace_record(si, decision, reason, row_id \\ nil) do
    Trace.append(si, :reflective_accountability,
      decision: decision,
      reason: reason,
      meta: %{
        ledger_source: @source,
        ledger_id: row_id,
        evidence_type: :lexical_role_diagnostic,
        cognition_modified?: false,
        learning_applied?: false
      }
    )
  end
end
