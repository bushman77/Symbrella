defmodule Core.Response.VerifiedPopulation do
  @moduledoc """
  Bounded, same-session retention of *retrieved dated population statements*.

  Separate from the temporary comprehension frame and from conversation text.
  Source assertions and their dates are retained, never an LLM reply. A cache
  hit cannot elevate an unsupported, undated or mismatched entity to a fact.
  """

  alias Core.Response.LlmChatHistoryOwner

  @table :core_verified_population_context
  @ttl_ms 10 * 60 * 1_000
  @max_sessions 256
  @max_subjects 4

  @spec record(term(), term(), keyword()) :: :ok
  def record(session_id, evidence, opts \\ [])

  def record(session_id, %{resolution: resolution,
                           support: %{kind: :population_group, subjects: names,
                                      results: results}}, opts)
      when resolution in [:source_supported, :partial_support] and
           is_list(names) and is_list(results) and is_list(opts) do
    with true <- valid_session?(session_id),
         true <- length(names) in 2..@max_subjects,
         true <- length(results) == length(names),
         true <- Enum.map(results, &Map.get(&1, :subject)) == names,
         true <- Enum.all?(results, &valid_entry?/1),
         true <- Enum.any?(results, &(&1.status == :source_supported)) do
      LlmChatHistoryOwner.ensure_table!()
      stamp = Keyword.get(opts, :now_ms, System.monotonic_time(:millisecond))

      # Drop any untrusted extra data from the provider's response.
      entries =
        Enum.map(results, fn entry ->
          if entry.status == :source_supported do
            Map.take(entry, [
              :subject, :status, :value, :census_year, :source_url, :source_type
            ])
          else
            %{subject: entry.subject, status: :unavailable}
          end
        end)

      :ets.insert(@table, {session_id, stamp, names, entries})
      prune(stamp)
    end

    :ok
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end

  def record(_, _, _), do: :ok

  @doc "Return only the requested, complete, same-year-or-not dated source claims."
  @spec recall(term(), list(), keyword()) :: map() | nil
  def recall(session_id, subjects, opts \\ [])

  def recall(session_id, subjects, opts)
      when is_list(subjects) and is_list(opts) do
    if valid_session?(session_id) and length(subjects) in 2..@max_subjects do
      LlmChatHistoryOwner.ensure_table!()
      now = Keyword.get(opts, :now_ms, System.monotonic_time(:millisecond))

      case :ets.lookup(@table, session_id) do
        [{^session_id, timestamp, names, entries}]
        when is_integer(timestamp) and is_list(names) and is_list(entries) ->
          if now >= timestamp and now - timestamp <= @ttl_ms and
               Enum.sort(names) == Enum.sort(subjects) and
               length(names) == length(subjects) and
               length(Enum.uniq(subjects)) == length(subjects) and
               Enum.all?(entries, &verified_entry?/1) do
            ordered = Enum.map(subjects, fn name ->
              Enum.find(entries, &(&1.subject == name))
            end)

            if Enum.all?(ordered, &verified_entry?/1) do
              sources = Enum.map(ordered, fn e ->
                %{
                  title: e.subject, url: e.source_url, provider: e[:source_type],
                  excerpt: "Dated population statement (#{e.census_year}): #{e.value}"
                }
              end)

              %{
                status: :retrieved,
                resolution: :source_supported,
                provider: :retained_verified_population,
                relation: :population,
                query: Enum.join(subjects, " and ") <> " population comparison",
                support: %{
                  kind: :population_group,
                  status: :source_supported,
                  subjects: subjects,
                  results: ordered
                },
                sources: sources,
                attempts: [],
                attempted_count: 0,
                routing_trigger: :same_session_verified_evidence,
                evidence_origin: :prior_verified_population
              }
            end
          end

        _ -> nil
      end
    end
  rescue
    _ -> nil
  catch
    :exit, _ -> nil
  end

  def recall(_, _, _), do: nil

  defp verified_entry?(%{
         subject: subject, status: :source_supported,
         value: value, census_year: year, source_url: url
       })
       when is_binary(subject) and is_integer(value) and value > 0 and
            is_integer(year) and year >= 1900 and is_binary(url) do
    String.starts_with?(url, "https://") and
      byte_size(subject) > 0 and byte_size(subject) <= 100 and byte_size(url) <= 500
  end

  defp verified_entry?(_), do: false

  defp valid_entry?(%{subject: subject, status: :unavailable})
       when is_binary(subject), do: byte_size(subject) > 0 and byte_size(subject) <= 100

  defp valid_entry?(entry), do: verified_entry?(entry)

  defp valid_session?(session_id),
    do: is_binary(session_id) and byte_size(session_id) > 0 and byte_size(session_id) <= 128

  defp prune(now) do
    entries = :ets.tab2list(@table)

    Enum.each(entries, fn
      {session, ts, _, _} when is_integer(ts) and (ts > now or now - ts > @ttl_ms) ->
        :ets.delete(@table, session)
      _ -> :ok
    end)

    remaining = :ets.tab2list(@table)

    if length(remaining) > @max_sessions do
      remaining
      |> Enum.sort_by(&elem(&1, 1))
      |> Enum.take(length(remaining) - @max_sessions)
      |> Enum.each(fn {session, _, _, _} -> :ets.delete(@table, session) end)
    end
  end
end
