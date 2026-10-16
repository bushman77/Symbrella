defmodule Core.Brain.WM.Comprehension do
  @moduledoc """
  Session-scoped, temporary conversational focus.

  A completed comprehension is eligible for WM as an *active context frame*,
  not a verified fact. This uses the canonical Brain.focus/2 admission path
  with permissive thresholds for this single candidate class only. Lexical
  candidates, grounded facts and recalled episodes keep their existing gates.

  Read the last same-session frame before discourse resolution; write the
  current frame after Core.Comprehension.Summary. No raw sentence is retained
  in the frame, and no cross-session fallback is permitted.
  """

  alias Core.Brain.Runtime
  alias Core.Pipeline.Trace

  @max_age_ms 20 * 60 * 1_000
  @max_entities 4
  @max_terms 6

  @spec recall(map(), keyword()) :: map()
  def recall(%{} = si, opts \\ []) when is_list(opts) do
    with true <- Keyword.get(opts, :comprehension_wm?, true),
         session when not is_nil(session) <- field(si, :session_id),
         %{} = item <- find_frame(session),
         %{} = context <- field(item, :context) do
      si
      |> Map.put(:wm_context, context)
      |> maybe_bind_plural_reference(context)
      |> Trace.append(:wm_context,
        decision: :recalled,
        reason: :recent_same_session_comprehension,
        meta: %{entity_count: length(List.wrap(context[:entities])), source: :comprehension}
      )
    else
      _ -> si
    end
  end

  def recall(si, _opts), do: si

  @spec store(map(), keyword()) :: map()
  def store(%{} = si, opts \\ []) when is_list(opts) do
    with true <- Keyword.get(opts, :comprehension_wm?, true),
         session when not is_nil(session) <- field(si, :session_id),
         %{} = comprehension <- field(si, :comprehension),
         sentence when is_binary(sentence) and byte_size(sentence) > 0 <- field(si, :sentence) do
      context = context_from(si, comprehension)
      id = frame_id(session)

      # :score here is focus priority, NEVER factual confidence. Keep lexical
      # :lifg_min_score and generic evidence floors unchanged. Only this
      # session's temporary comprehension frame gets a permissive gate.
      candidate = %{
        id: id,
        lemma: "current comprehension",
        source: :comprehension,
        score: 0.6,
        reason: :current_conversation,
        context: context,
        kind: :comprehension_frame
      }

      result =
        safe_brain_call(:focus, [
          [candidate],
          [
            gate_threshold: 0.0,
            boost_threshold: 0.0,
            boost_threshold_pref: 0.0,
            block_threshold: 0.0,
            evidence_floor: 0.0
          ]
        ])

      admitted? =
        is_list(result) and
          Enum.any?(result, &(field(&1, :id) == id))

      :telemetry.execute(
        [:core, :wm, :comprehension],
        %{candidates: 1, admitted: if(admitted?, do: 1, else: 0)},
        %{decision: if(admitted?, do: :admitted, else: :unavailable), source: :comprehension}
      )

      Trace.append(si, :wm_comprehension,
        decision: if(admitted?, do: :admitted, else: :unavailable),
        reason: :completed_comprehension_focus,
        meta: %{
          candidates: 1,
          admitted: if(admitted?, do: 1, else: 0),
          entity_count: length(context.entities),
          degraded?: context.degraded?,
          fact_verified?: false
        }
      )
    else
      _ -> si
    end
  end

  def store(si, _opts), do: si

  @doc "Build a small frame from current comprehension and independently identified event roles."
  @spec context_from(map(), map()) :: map()
  def context_from(si, comprehension) when is_map(si) and is_map(comprehension) do
    event = map_or_empty(field(si, :symbolic_frame))
    prev = map_or_empty(field(si, :wm_context))
    reference = map_or_empty(field(si, :discourse_reference))
    sentence = field(si, :sentence) || ""

    explicit_entities =
      [field(event, :subject), field(event, :target)]
      |> Enum.filter(&is_binary/1)
      |> Enum.map(&clean_entity/1)
      |> Enum.reject(&(&1 == "" or pronoun?(&1)))
      |> Enum.uniq()
      |> Enum.take(@max_entities)

    inherited =
      if plural_reference?(sentence, reference) and explicit_entities == [] do
        List.wrap(field(prev, :entities))
        |> Enum.filter(&is_binary/1)
        |> Enum.take(@max_entities)
      else
        []
      end

    entities = if explicit_entities == [], do: inherited, else: explicit_entities

    relation =
      field(event, :relation) ||
        if(inherited != [] and Regex.match?(~r/\bpopulations?\b/iu, sentence),
          do: :population, else: nil)

    %{
      schema: 1,
      intent: field(si, :intent),
      entities: entities,
      relation: relation,
      recognized_topics: terms(field(comprehension, :recognized_topics)),
      understood: terms(field(comprehension, :understood)),
      uncertain: terms(field(comprehension, :uncertain)),
      degraded?: field(comprehension, :degraded?) == true,
      interpretation_status: field(map_or_empty(field(si, :sentence_meaning)), :status),
      verification: :conversation_context_only,
      updated_at_ms: System.system_time(:millisecond)
    }
  end

  defp maybe_bind_plural_reference(si, %{entities: entities} = _context) when is_list(entities) do
    sentence = field(si, :sentence)

    cond do
      not is_binary(sentence) or field(si, :discourse_reference) != nil ->
        si

      length(entities) >= 2 and
          Regex.match?(~r/\b(?:each|both)\b/iu, sentence) ->
        Map.put(si, :discourse_reference, %{
          pronoun: if(Regex.match?(~r/\bboth\b/iu, sentence), do: "both", else: "each"),
          antecedents: Enum.take(entities, @max_entities),
          source: :wm_comprehension,
          evidence: :prior_same_session_comprehension,
          verification: :conversation_context_only
        })

      true -> si
    end
  end

  defp maybe_bind_plural_reference(si, _), do: si

  defp plural_reference?(sentence, reference) do
    is_binary(sentence) and Regex.match?(~r/\b(?:each|both)\b/iu, sentence) and
      field(reference, :source) == :wm_comprehension
  end

  defp find_frame(session) do
    case safe_brain_call(:snapshot_wm, []) do
      %{wm: wm} when is_list(wm) ->
        id = frame_id(session)
        now = System.system_time(:millisecond)

        Enum.find_value(wm, fn item ->
          ts = field(item, :last_bump) || field(item, :ts)
          payload = field(item, :payload)

          if field(item, :id) == id and is_map(payload) and
               field(payload, :kind) == :comprehension_frame and
               is_integer(ts) and now >= ts and now - ts <= @max_age_ms do
            payload
          end
        end)

      _ ->
        nil
    end
  end

  defp frame_id(session) do
    digest =
      :crypto.hash(:sha256, :erlang.term_to_binary({:symbrella_wm_session, session}))
      |> Base.encode16(case: :lower)
      |> binary_part(0, 32)

    "comprehension:#{digest}"
  end

  defp safe_brain_call(function, args) do
    if Runtime.pid_alive?(Brain) do
      Runtime.apply_if_exported(Brain, function, args, nil)
    end
  rescue
    _ -> nil
  catch
    :exit, _ -> nil
  end

  defp terms(value) do
    value
    |> List.wrap()
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.slice(String.trim(&1), 0, 96))
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.take(@max_terms)
  end

  defp clean_entity(text) do
    text
    |> String.trim()
    |> String.slice(0, 96)
    |> String.replace(~r/\s+/u, " ")
  end

  defp pronoun?(text),
    do: String.downcase(text) in ~w(it this that each both there they them its)

  defp map_or_empty(%{} = value), do: value
  defp map_or_empty(_), do: %{}

  defp field(%{} = map, key), do: Map.get(map, key, Map.get(map, Atom.to_string(key)))
  defp field(_, _), do: nil
end
