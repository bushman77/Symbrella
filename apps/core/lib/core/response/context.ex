defmodule Core.Response.Context do
  @moduledoc """
  Builds the compact response context that sits between symbolic Core/Brain state
  and prompt rendering.

  This module intentionally keeps the context bounded. It does not expose raw SI
  payloads to the LLM prompt; it summarizes the parts that should shape response
  behavior.
  """

  @summary_limit 5

  @type t :: map()

  @spec from_si(map()) :: t()
  def from_si(%{} = si) do
    %{
      user_text: si_text(si),
      session_id: map_get(si, :session_id, :global),
      symbolic_frame: symbolic_frame(si),
      comprehension: map_get(si, :comprehension),
      self_model: map_get(si, :self_model)
    }
    |> drop_empty()
  end

  def from_si(_), do: %{}

  @spec from_response_parts(String.t(), map(), map(), map(), map()) :: t()
  def from_response_parts(user_text, features, decision, mood, runtime)
      when is_map(features) and is_map(decision) and is_map(mood) and is_map(runtime) do
    embedded = embedded_context(features)

    %{
      user_text: user_text,
      features: features,
      decision: decision,
      mood: mood,
      wm_items: map_get(runtime, :wm_items, []),
      self_model:
        first_present([
          map_get(features, :self_model),
          map_get(embedded, :self_model),
          map_get(runtime, :self_model)
        ]),
      runtime_state: map_get(runtime, :runtime_state),
      comprehension:
        first_present([
          map_get(features, :comprehension),
          map_get(embedded, :comprehension)
        ]),
      symbolic_frame:
        first_present([
          map_get(features, :symbolic_frame),
          map_get(embedded, :symbolic_frame)
        ]),
      session_id:
        first_present([
          map_get(features, :session_id),
          map_get(embedded, :session_id),
          :global
        ])
    }
    |> drop_empty()
  end

  def from_response_parts(user_text, features, decision, mood, runtime) do
    from_response_parts(
      user_text,
      ensure_map(features),
      ensure_map(decision),
      ensure_map(mood),
      ensure_map(runtime)
    )
  end

  @spec symbolic_frame(map()) :: map()
  def symbolic_frame(%{} = si) do
    %{
      intent: map_get(si, :intent),
      keyword: map_get(si, :keyword),
      confidence: map_get(si, :confidence),
      lexical: lexical_summary(si),
      lifg: lifg_summary(si),
      perception: present_summary(map_get(si, :perception)),
      atl_slate: present_summary(map_get(si, :atl_slate)),
      episode: present_summary(map_get(si, :episode)),
      self_model: present_summary(map_get(si, :self_model))
    }
    |> drop_empty()
  end

  def symbolic_frame(_), do: %{}

  defp embedded_context(features) do
    case map_get(features, :turn_context) do
      %{} = context -> context
      _ -> %{}
    end
  end

  defp lexical_summary(si) do
    tokens = list_or_empty(map_get(si, :tokens))
    active_cells = list_or_empty(map_get(si, :active_cells))
    sense_candidates = map_get(si, :sense_candidates, %{})
    mwe_matches = list_or_empty(map_get(si, :mwe_matches))

    %{
      token_count: length(tokens),
      active_cells_count: length(active_cells),
      sense_candidates_count: sense_candidate_count(sense_candidates),
      mwe_matches_count: length(mwe_matches),
      top_terms: top_terms(tokens, active_cells)
    }
    |> drop_empty()
  end

  defp lifg_summary(si) do
    choices = list_or_empty(map_get(si, :lifg_choices))
    acc_conflict = map_get(si, :acc_conflict)

    degraded? =
      is_number(acc_conflict) and acc_conflict >= 0.5

    %{
      choices_count: length(choices),
      acc_conflict: acc_conflict,
      degraded?: degraded?
    }
    |> drop_empty()
  end

  defp top_terms(tokens, active_cells) do
    (tokens ++ active_cells)
    |> Enum.map(&term_label/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
    |> Enum.take(@summary_limit)
  end

  defp term_label(%{} = item) do
    payload = map_get(item, :payload)

    payload_term =
      if is_map(payload) do
        map_get(payload, :lemma) || map_get(payload, :norm)
      end

    normalize_term(
      payload_term ||
        map_get(item, :lemma) ||
        map_get(item, :norm) ||
        map_get(item, :phrase) ||
        map_get(item, :surface) ||
        map_get(item, :text) ||
        map_get(item, :id)
    )
  end

  defp term_label(term), do: normalize_term(term)

  defp normalize_term(nil), do: ""

  defp normalize_term(term) when is_binary(term) do
    term
    |> String.trim()
    |> clamp(64)
  end

  defp normalize_term(term) do
    term
    |> to_string()
    |> normalize_term()
  end

  defp sense_candidate_count(candidates) when is_map(candidates) do
    candidates
    |> Map.values()
    |> Enum.reduce(0, fn
      list, acc when is_list(list) -> acc + length(list)
      _other, acc -> acc
    end)
  end

  defp sense_candidate_count(list) when is_list(list), do: length(list)
  defp sense_candidate_count(_), do: 0

  defp present_summary(nil), do: nil
  defp present_summary(map) when map == %{}, do: nil
  defp present_summary(list) when is_list(list) and list == [], do: nil
  defp present_summary(%{}), do: :present
  defp present_summary(list) when is_list(list), do: "present:#{length(list)}"
  defp present_summary(_), do: :present

  defp si_text(si) do
    map_get(si, :sentence) ||
      map_get(si, :text) ||
      map_get(si, :keyword) ||
      ""
  end

  defp first_present(values) do
    Enum.find(values, fn
      nil -> false
      "" -> false
      %{} = map -> map_size(map) > 0
      [] -> false
      _ -> true
    end)
  end

  defp list_or_empty(list) when is_list(list), do: list
  defp list_or_empty(_), do: []

  defp drop_empty(map) when is_map(map) do
    map
    |> Enum.reject(fn
      {_key, nil} -> true
      {_key, ""} -> true
      {_key, []} -> true
      {_key, %{} = nested} -> map_size(nested) == 0
      {_key, false} -> true
      {_key, _value} -> false
    end)
    |> Enum.into(%{})
  end

  defp clamp(text, max) when is_binary(text) and is_integer(max) do
    if String.length(text) > max, do: String.slice(text, 0, max), else: text
  end

  defp ensure_map(map) when is_map(map), do: map
  defp ensure_map(_), do: %{}

  defp map_get(map, key, default \\ nil)

  defp map_get(map, key, default) when is_map(map) and is_atom(key) do
    Map.get(map, key, Map.get(map, Atom.to_string(key), default))
  end

  defp map_get(_, _, default), do: default
end
