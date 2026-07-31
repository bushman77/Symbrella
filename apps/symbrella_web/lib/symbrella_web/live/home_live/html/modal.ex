# apps/symbrella_web/lib/symbrella_web/live/home_live/html/modal.ex
defmodule SymbrellaWeb.HomeLive.HTML.Modal do
  @moduledoc """
  Single-panel, mobile-first full-width modal for chat explanations.

  Constrained between header/footer using CSS vars:
    --hdr / --ftr

  Close button is TOP ONLY (sticky).
  Body is scrollable.
  Always renders as ONE column (no split panes).
  """

  use SymbrellaWeb, :html

  # ---------- component ----------

  attr :open?, :boolean, default: false
  attr :payload, :map, default: %{}

  def explain_modal(assigns) do
    ~H"""
    <% p = normalize_payload(@payload) %>
    <% sections = pget(p, :sections, []) || [] %>

    <div
      :if={@open?}
      class="fixed z-[9999] bg-white overflow-x-hidden"
      style={
        "top: var(--hdr,56px); bottom: var(--ftr,72px);" <>
          "left: 50%; width: 100vw; margin-left: -50vw;"
      }
      role="dialog"
      aria-modal="true"
      aria-label="Explain"
      phx-window-keydown="explain_close"
      phx-key="escape"
    >
      <div class="h-full w-full bg-white">
        <div class="h-full w-full max-w-none flex flex-col">
          <div class="sticky top-0 z-10 border-b border-slate-200 bg-white px-4 py-3">
            <div class="flex items-center justify-end">
              <button
                type="button"
                class="rounded-xl border border-slate-300 bg-slate-900 px-3 py-1.5 text-xs font-semibold text-white"
                phx-click="explain_close"
                aria-label="Close"
              >
                Close
              </button>
            </div>
          </div>

          <div class="flex-1 min-h-0 overflow-y-auto overscroll-contain px-4 py-4 overflow-x-hidden">
            <div class="grid gap-3 w-full">
              <div
                :for={sec <- sections}
                class="rounded-2xl border border-slate-200 bg-slate-50 p-3"
              >
                <div class="flex items-center justify-between gap-3">
                  <div class="text-sm font-semibold truncate">
                    {sec[:title] || "Section"}
                  </div>

                  <span
                    :if={sec[:tag]}
                    class="shrink-0 rounded-full border border-slate-200 bg-white px-2 py-0.5 text-[11px] text-slate-700"
                  >
                    {sec[:tag]}
                  </span>
                </div>

                <div :if={sec[:hint]} class="mt-1 text-xs text-slate-600">
                  {sec[:hint]}
                </div>

                <div class="mt-2 grid gap-2">
                  <div
                    :for={item <- sec[:items] || []}
                    class="rounded-xl border border-slate-200 bg-white p-3"
                  >
                    <div :if={item[:label]} class="text-xs font-semibold text-slate-800">
                      {item[:label]}
                    </div>

                    <div class="mt-1 text-sm whitespace-pre-wrap break-words text-slate-700">
                      {item[:body] || ""}
                    </div>
                  </div>

                  <div :if={(sec[:items] || []) == []} class="text-sm text-slate-500">
                    No details for this section.
                  </div>
                </div>
              </div>

              <div :if={sections == []} class="text-sm text-slate-500">
                No explanation available for this message.
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    """
  end

  # ---------- builder ----------

  @doc """
  Builds a sectioned explanation payload from a message-like map.

  Optional (preferred for "Senses selected"):
    :senses_selected => [%{label: "...", definition: "..."}, ...]
  """
  def explain_payload_for(message) when is_map(message) do
    text = to_string(message[:text] || "")
    {main0, extra0} = split_lexical_tail(text)

    # 1) meta line: try tail first, then main (some pipelines append meta into main)
    {meta_line1, extra_wo_meta} = extract_meta_line(extra0 || "")

    {meta_line2, main_wo_meta} =
      if meta_line1 == nil, do: extract_meta_line(main0), else: {nil, main0}

    meta_line = meta_line1 || meta_line2
    main1 = if meta_line1 == nil and meta_line2 != nil, do: main_wo_meta, else: main0

    meta = merge_meta(message, parse_meta_line(meta_line))

    # 2) strip the leading tone line from "Message" content (tone already has its own section)
    main =
      main1
      |> strip_leading_tone(meta[:tone])
      |> String.trim()

    structured_senses =
      message[:senses_selected] ||
        get_in(message, [:from, :senses_selected]) ||
        []

    from = pget(message, :from, %{})

    symbolic_frame =
      pget(message, :symbolic_frame) ||
        pget(from, :symbolic_frame)

    selected_action =
      pget(message, :selected_action) ||
        pget(from, :selected_action) ||
        pget(from, :agent_selected_action)

    action_candidates =
      pget(message, :action_candidates) ||
        pget(from, :action_candidates) ||
        pget(from, :agent_action_candidates) ||
        []

    action_meta =
      pget(message, :action_meta) ||
        pget(from, :action_meta) ||
        pget(from, :agent_action_meta) ||
        %{}

    bullets =
      extra_wo_meta
      |> parse_reading_bullets()
      |> then(fn bs -> if bs == [], do: parse_reading_bullets(text), else: bs end)

    sections =
      []
      |> maybe_add_intent_tone_section(meta, meta_line)
      |> maybe_add_senses_selected_section(structured_senses, bullets)
      |> maybe_add_event_frame_section(symbolic_frame)
      |> maybe_add_action_selection_section(selected_action, action_candidates, action_meta)
      |> maybe_add_similar_terms_section(bullets)
      |> maybe_add_antonyms_section(bullets)
      |> maybe_add_message_section(main)

    %{
      title: "Explain",
      subtitle: subtitle_for(message, meta),
      sections: sections
    }
  end

  # ---------- payload normalizer ----------

  defp pget(map, key, default \\ nil)

  defp pget(map, key, default) when is_map(map) do
    k2 = Atom.to_string(key)

    cond do
      Map.has_key?(map, key) -> Map.get(map, key)
      Map.has_key?(map, k2) -> Map.get(map, k2)
      true -> default
    end
  end

  defp pget(_map, _key, default), do: default

  defp normalize_payload(payload) when is_map(payload) do
    sections = pget(payload, :sections)

    cond do
      is_list(sections) and sections != [] ->
        payload

      true ->
        text = pget(payload, :text, "")
        from = pget(payload, :from, %{})

        msg = %{
          id: pget(payload, :id, "pending"),
          text: text,
          from: from,
          intent: pget(payload, :intent),
          confidence: pget(payload, :confidence),
          tone: pget(payload, :tone),
          senses_selected: pget(payload, :senses_selected),
          symbolic_frame: pget(payload, :symbolic_frame) || pget(from, :symbolic_frame),
          selected_action:
            pget(payload, :selected_action) ||
              pget(from, :selected_action) ||
              pget(from, :agent_selected_action),
          action_candidates:
            pget(payload, :action_candidates) ||
              pget(from, :action_candidates) ||
              pget(from, :agent_action_candidates),
          action_meta:
            pget(payload, :action_meta) ||
              pget(from, :action_meta) ||
              pget(from, :agent_action_meta)
        }

        built = explain_payload_for(msg)

        built
        |> Map.put(:title, pget(payload, :title, built[:title]))
        |> Map.put(:subtitle, pget(payload, :subtitle, built[:subtitle]))
    end
  end

  defp normalize_payload(_), do: %{title: "Explain", subtitle: "Details", sections: []}

  # ---------- subtitle/meta ----------

  defp subtitle_for(_message, meta) do
    intent = meta[:intent] && ":#{meta[:intent]}"
    conf = meta[:confidence] && format_conf(meta[:confidence])
    tone = meta[:tone] && ":#{meta[:tone]}"

    pieces =
      [intent && "intent=#{intent}", conf && "conf=#{conf}", tone && "tone=#{tone}"]
      |> Enum.filter(&(&1 && &1 != ""))

    case pieces do
      [] -> "Details"
      xs -> Enum.join(xs, " · ")
    end
  end

  defp format_conf(v) when is_float(v), do: :erlang.float_to_binary(v, decimals: 2)
  defp format_conf(v) when is_integer(v), do: Integer.to_string(v)
  defp format_conf(v) when is_binary(v), do: v
  defp format_conf(_), do: nil

  # ---------- lexical tail + meta line ----------

  defp split_lexical_tail(text) when is_binary(text) do
    needle = "By the way,"

    case :binary.match(text, needle) do
      {pos, _len} when is_integer(pos) and pos > 0 ->
        left = :binary.part(text, 0, pos) |> String.trim_trailing()
        right = :binary.part(text, pos, byte_size(text) - pos)
        {left, right}

      _ ->
        {text, nil}
    end
  end

  defp split_lexical_tail(other), do: {to_string(other), nil}

  # NEW: if the first line is a tone label ("warm"), drop it from Message display
  defp strip_leading_tone(text, tone) when is_binary(text) do
    t =
      cond do
        is_atom(tone) -> Atom.to_string(tone)
        is_binary(tone) -> tone |> String.trim() |> String.trim_leading(":")
        true -> nil
      end

    if is_binary(t) and t != "" do
      lines = String.split(text, "\n", trim: false)

      case lines do
        [first | rest] ->
          f = String.trim(first)

          if f == t or f == ":" <> t do
            rest |> Enum.join("\n") |> String.trim_leading()
          else
            text
          end

        _ ->
          text
      end
    else
      text
    end
  end

  defp strip_leading_tone(other, _), do: to_string(other)

  defp extract_meta_line(extra) when is_binary(extra) do
    lines = String.split(extra, "\n", trim: false)

    {meta_line, idx} =
      lines
      |> Enum.with_index()
      |> Enum.reverse()
      |> Enum.find_value({nil, nil}, fn {line, i} ->
        t = String.trim(line)

        if String.contains?(t, "intent=:") or String.starts_with?(t, "intent="),
          do: {t, i},
          else: false
      end)

    if meta_line && idx != nil do
      remaining =
        lines
        |> Enum.with_index()
        |> Enum.reject(fn {_line, i} -> i == idx end)
        |> Enum.map(fn {line, _i} -> line end)
        |> Enum.join("\n")

      {meta_line, remaining}
    else
      {nil, extra}
    end
  end

  defp extract_meta_line(other), do: {nil, to_string(other)}

  defp parse_meta_line(nil), do: %{}

  defp parse_meta_line(line) when is_binary(line) do
    r =
      Regex.named_captures(
        ~r/intent=:(?<intent>[\w_]+)\((?<confidence>[\d.]+)\).*?tone=:(?<tone>[\w_]+)\s+because=(?<because>[^·\n]+)(?:\s+·\s+mode=(?<mode>[^\s\n]+))?/u,
        line
      )

    if is_map(r) do
      %{
        intent: safe_atom(r["intent"]),
        confidence: safe_float(r["confidence"]),
        tone: safe_atom(r["tone"]),
        because: String.trim(to_string(r["because"] || "")),
        mode: String.trim(to_string(r["mode"] || "")),
        raw: line
      }
      |> drop_blank(:because)
      |> drop_blank(:mode)
    else
      %{raw: line}
    end
  end

  defp merge_meta(message, parsed) do
    from = message[:from] || %{}

    %{
      intent: message[:intent] || from[:intent_inferred] || from[:intent] || parsed[:intent],
      confidence: message[:confidence] || from[:confidence] || parsed[:confidence],
      tone: message[:tone] || from[:tone_reaction] || parsed[:tone],
      because: message[:tone_because] || from[:source_latents] || parsed[:because],
      mode: message[:mode] || parsed[:mode],
      response_source: message[:response_source] || from[:response_source],
      response_fallback_reason:
        message[:response_fallback_reason] || from[:response_fallback_reason],
      action: message[:action] || from[:action],
      memory_key: message[:memory_key] || from[:memory_key] || from[:fact_key],
      memory_source: message[:memory_source] || from[:memory_source] || from[:source],
      raw: parsed[:raw]
    }
    |> drop_blank(:because)
    |> drop_blank(:mode)
    |> drop_nil(:raw)
  end

  defp drop_blank(map, key) do
    case Map.get(map, key) do
      v when is_binary(v) ->
        if String.trim(v) == "", do: Map.delete(map, key), else: map

      _ ->
        map
    end
  end

  defp drop_nil(map, key), do: if(Map.get(map, key) == nil, do: Map.delete(map, key), else: map)

  defp safe_atom(nil), do: nil
  defp safe_atom(v) when is_atom(v), do: v

  defp safe_atom(v) when is_binary(v) do
    s = String.trim(v)

    cond do
      s == "" -> nil
      Regex.match?(~r/^[a-z_]+$/u, s) -> String.to_atom(s)
      true -> s
    end
  end

  defp safe_atom(v), do: safe_atom(to_string(v))

  defp safe_float(nil), do: nil
  defp safe_float(v) when is_float(v), do: v
  defp safe_float(v) when is_integer(v), do: v / 1

  defp safe_float(v) when is_binary(v) do
    case Float.parse(String.trim(v)) do
      {f, ""} -> f
      _ -> nil
    end
  end

  defp safe_float(v), do: safe_float(to_string(v))

  # ---------- bullet parsing (fallback) ----------

  defp parse_reading_bullets(extra) when is_binary(extra) do
    extra
    |> String.split("\n", trim: false)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.filter(&bullet_line?/1)
    |> Enum.map(&strip_bullet_prefix/1)
    |> Enum.map(&parse_reading_bullet_content/1)
    |> Enum.reject(&is_nil/1)
  end

  defp parse_reading_bullets(_), do: []

  defp bullet_line?(line) when is_binary(line), do: Regex.match?(~r/^(?:•|·|-|\*)\s*/u, line)
  defp bullet_line?(_), do: false

  defp strip_bullet_prefix(line) when is_binary(line) do
    Regex.replace(~r/^(?:•|·|-|\*)\s*/u, line, "")
    |> String.trim()
  end

  defp strip_bullet_prefix(other), do: to_string(other)

  defp parse_reading_bullet_content(rest) when is_binary(rest) do
    rest = String.trim(rest)
    parts = split_dash(rest, 3)

    {head, desc0, qual0} =
      case parts do
        [h, d, q] -> {String.trim(h), String.trim(d), String.trim(q)}
        [h, d] -> {String.trim(h), String.trim(d), nil}
        [h] -> {String.trim(h), "", nil}
        _ -> {rest, "", nil}
      end

    {token, pos} =
      case Regex.named_captures(~r/^(?<token>.+?)\s*\((?<pos>[^)]+)\)\s*$/u, head) do
        %{"token" => t, "pos" => p} -> {String.trim(t), String.trim(p)}
        _ -> {head, nil}
      end

    {desc, sim_from_desc} =
      if String.contains?(String.downcase(desc0), "similar to") do
        split_similar_phrase(desc0)
      else
        {desc0, nil}
      end

    {sim_from_qual, ant_from_qual} = parse_qualifiers(qual0)

    similar = join_lines([sim_from_desc, sim_from_qual])
    antonyms = ant_from_qual

    label = if pos, do: "#{token} (#{pos})", else: token

    %{
      token: token,
      pos: pos,
      label: label,
      desc: blank_to_nil(desc),
      similar: blank_to_nil(similar),
      antonyms: blank_to_nil(antonyms)
    }
  end

  defp parse_reading_bullet_content(_), do: nil

  defp blank_to_nil(v) when is_binary(v), do: if(String.trim(v) == "", do: nil, else: v)
  defp blank_to_nil(_), do: nil

  defp split_similar_phrase(desc0) do
    re = ~r/^(?<left>.*?)\s*(?:—|–|-)\s*similar to:?\s*(?<right>.*)$/iu

    case Regex.named_captures(re, desc0) do
      %{"left" => left, "right" => right} ->
        l = String.trim(left)
        r = String.trim(right)
        {l, if(r == "", do: nil, else: "Similar to: #{r}")}

      _ ->
        {desc0, nil}
    end
  end

  defp parse_qualifiers(nil), do: {nil, nil}

  defp parse_qualifiers(q) when is_binary(q) do
    clauses =
      q
      |> String.split([";", "·"], trim: true)
      |> Enum.map(&String.trim/1)
      |> Enum.reject(&(&1 == ""))

    Enum.reduce(clauses, {nil, nil}, fn clause, {sacc, aacc} ->
      down = String.downcase(clause)

      cond do
        String.starts_with?(down, "similar to") ->
          v = clause |> strip_prefix("similar to") |> String.trim(" :")
          {append_line(sacc, "Similar to: #{v}"), aacc}

        String.starts_with?(down, "synonyms") ->
          v = clause |> strip_prefix("synonyms") |> String.trim(" :")
          {append_line(sacc, "Similar to: #{v}"), aacc}

        String.starts_with?(down, "opposite of") ->
          v = clause |> strip_prefix("opposite of") |> String.trim(" :")
          {sacc, append_line(aacc, "Opposite of: #{v}")}

        String.starts_with?(down, "antonyms") ->
          v = clause |> strip_prefix("antonyms") |> String.trim(" :")
          {sacc, append_line(aacc, "Opposite of: #{v}")}

        String.starts_with?(down, "antonym of") ->
          v = clause |> strip_prefix("antonym of") |> String.trim(" :")
          {sacc, append_line(aacc, "Opposite of: #{v}")}

        true ->
          {sacc, aacc}
      end
    end)
  end

  defp strip_prefix(str, prefix) do
    n = String.length(prefix)
    String.slice(str, n..-1//1) || ""
  end

  defp append_line(nil, line), do: line
  defp append_line(acc, line) when is_binary(acc), do: acc <> "\n" <> line

  defp join_lines(list) do
    list
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join("\n")
  end

  defp split_dash(s, max_parts) when is_binary(s) and is_integer(max_parts) do
    cond do
      String.contains?(s, " — ") -> String.split(s, " — ", parts: max_parts)
      String.contains?(s, "—") -> String.split(s, "—", parts: max_parts)
      String.contains?(s, " – ") -> String.split(s, " – ", parts: max_parts)
      String.contains?(s, " - ") -> String.split(s, " - ", parts: max_parts)
      true -> [s]
    end
  end

  # ---------- sections ----------

  defp maybe_add_intent_tone_section(sections, meta, meta_line) do
    has_any? =
      meta[:intent] || meta[:confidence] || meta[:tone] || meta[:because] || meta[:mode] ||
        meta[:response_source] || meta[:response_fallback_reason] || meta_line

    if has_any? do
      items =
        []
        |> maybe_kv("intent", meta[:intent] && ":#{meta[:intent]}")
        |> maybe_kv("confidence", meta[:confidence] && format_conf(meta[:confidence]))
        |> maybe_kv("tone", meta[:tone] && ":#{meta[:tone]}")
        |> maybe_kv("because", meta[:because])
        |> maybe_kv("mode", meta[:mode])
        |> maybe_kv("response source", meta[:response_source] && ":#{meta[:response_source]}")
        |> maybe_kv("action", meta[:action] && ":#{meta[:action]}")
        |> maybe_kv("memory key", format_frame_value(meta[:memory_key]))
        |> maybe_kv("memory source", meta[:memory_source] && ":#{meta[:memory_source]}")
        |> maybe_kv(
          "fallback reason",
          meta[:response_fallback_reason] && ":#{meta[:response_fallback_reason]}"
        )
        |> maybe_raw("raw line", meta[:raw], meta_line)

      sections ++
        [
          %{
            key: :intent_tone,
            title: "Intent & tone",
            hint: intent_tone_hint(meta),
            items: items
          }
        ]
    else
      sections
    end
  end

  defp intent_tone_hint(%{response_source: :memory}),
    do: "Memory attribution and tone metadata that shaped the reply."

  defp intent_tone_hint(%{response_source: "memory"}),
    do: "Memory attribution and tone metadata that shaped the reply."

  defp intent_tone_hint(_),
    do: "Classifier + tone selection metadata that shaped the reply."

  defp maybe_add_senses_selected_section(sections, structured_senses, bullets) do
    items_from_struct =
      structured_senses
      |> List.wrap()
      |> Enum.filter(&is_map/1)
      |> Enum.map(fn s ->
        label = s[:label] || s[:raw] || s[:token] || "Sense"
        defn = s[:definition] || s[:def] || s[:gloss] || s[:body] || ""
        ex = s[:example] || s[:ex] || nil
        body = sense_body(defn, ex)
        %{label: to_string(label), body: body}
      end)

    items =
      if items_from_struct != [] do
        items_from_struct
      else
        bullets
        |> Enum.map(fn b ->
          body =
            cond do
              is_binary(b[:desc]) and String.trim(b[:desc]) != "" -> b[:desc]
              true -> "(definition unavailable)"
            end

          %{label: b.label, body: body}
        end)
      end

    if items != [] do
      sections ++
        [
          %{
            key: :senses_selected,
            title: "Senses selected",
            tag: "#{length(items)}",
            hint: "Selected sense per token (with a best-available gloss).",
            items: items
          }
        ]
    else
      sections
    end
  end

  defp maybe_add_event_frame_section(sections, frame)
       when is_map(frame) and map_size(frame) > 0 do
    title =
      frame
      |> pget(:type, :event_frame)
      |> format_frame_value()

    items =
      frame
      |> event_frame_items()
      |> Enum.map(fn {label, value} ->
        %{label: label, body: format_frame_value(value)}
      end)

    sections ++
      [
        %{
          key: :event_frame,
          title: "Semantic event frame",
          tag: title,
          hint: "Structured meaning extracted from the selected senses and intent.",
          items: items
        }
      ]
  end

  defp maybe_add_event_frame_section(sections, _), do: sections

  defp event_frame_items(frame) when is_map(frame) do
    preferred = [
      :type,
      :subject,
      :event,
      :medication,
      :consequence,
      :temporal_context,
      :domain,
      :polarity,
      :confidence
    ]

    preferred_items =
      preferred
      |> Enum.filter(fn key -> frame_present?(pget(frame, key)) end)
      |> Enum.map(fn key -> {format_frame_key(key), pget(frame, key)} end)

    remaining_items =
      frame
      |> Enum.reject(fn {key, value} ->
        normalized_key = normalize_frame_key(key)
        normalized_key in preferred or not frame_present?(value)
      end)
      |> Enum.sort_by(fn {key, _value} -> to_string(key) end)
      |> Enum.map(fn {key, value} -> {format_frame_key(key), value} end)

    preferred_items ++ remaining_items
  end

  defp frame_present?(nil), do: false
  defp frame_present?(""), do: false
  defp frame_present?([]), do: false
  defp frame_present?(map) when is_map(map), do: map_size(map) > 0
  defp frame_present?(_), do: true
  defp normalize_frame_key(key) when is_atom(key), do: key

  defp normalize_frame_key(key) when is_binary(key) do
    key
    |> String.trim()
    |> String.to_existing_atom()
  rescue
    ArgumentError -> key
  end

  defp normalize_frame_key(key), do: key

  defp format_frame_key(key) when is_atom(key) do
    key
    |> Atom.to_string()
    |> String.replace("_", " ")
  end

  defp format_frame_key(key) when is_binary(key) do
    key
    |> String.trim()
    |> String.replace("_", " ")
  end

  defp format_frame_key(key), do: to_string(key)

  defp format_frame_value(nil), do: ""
  defp format_frame_value(value) when is_atom(value), do: ":#{value}"

  defp format_frame_value(value) when is_float(value) do
    :erlang.float_to_binary(value, decimals: 2)
  end

  defp format_frame_value(value) when is_integer(value), do: Integer.to_string(value)
  defp format_frame_value(value) when is_binary(value), do: value

  defp format_frame_value(value) when is_list(value) do
    value
    |> Enum.map(&format_frame_value/1)
    |> Enum.join(", ")
  end

  defp format_frame_value(value) when is_map(value) do
    value
    |> Enum.map(fn {k, v} -> "#{format_frame_key(k)}=#{format_frame_value(v)}" end)
    |> Enum.join("\n")
  end

  defp format_frame_value(value), do: inspect(value)

  defp maybe_add_action_selection_section(sections, selected_action, candidates, action_meta) do
    selected =
      selected_action ||
        pget(action_meta || %{}, :selected)

    candidates =
      case candidates do
        list when is_list(list) -> list
        _ -> pget(action_meta || %{}, :candidates, [])
      end
      |> List.wrap()
      |> Enum.filter(&is_map/1)

    if frame_present?(selected) or candidates != [] or frame_present?(action_meta) do
      items =
        []
        |> maybe_kv("selected action", selected && format_frame_value(selected))
        |> maybe_kv(
          "safety gate",
          action_meta |> pget(:safety_gate) |> maybe_format_frame_value()
        )
        |> maybe_kv("confidence", action_meta |> pget(:confidence) |> maybe_format_frame_value())
        |> maybe_kv("version", action_meta |> pget(:version) |> maybe_format_frame_value())
        |> maybe_kv("selected reason", selected_reason(action_meta))
        |> add_action_candidates(candidates)

      sections ++
        [
          %{
            key: :action_selection,
            title: "Action selection",
            tag: selected && format_frame_value(selected),
            hint: "Bounded internal/text action chosen before the response.",
            items: items
          }
        ]
    else
      sections
    end
  end

  defp add_action_candidates(items, candidates) when is_list(candidates) do
    if candidates == [] do
      items
    else
      body =
        candidates
        |> Enum.map(fn candidate ->
          action = candidate |> pget(:action) |> format_frame_value()
          score = candidate |> pget(:score) |> format_frame_value()
          reason = candidate |> pget(:reason) |> format_frame_value()

          speech? = pget(candidate, :speech_required?)
          memory? = pget(candidate, :memory_relevant?)

          extras =
            []
            |> maybe_inline_flag("speech", speech?)
            |> maybe_inline_flag("memory", memory?)
            |> Enum.join(", ")

          base = "#{action} score=#{score} reason=#{reason}"

          if extras == "" do
            base
          else
            base <> " [" <> extras <> "]"
          end
        end)
        |> Enum.join("\n")

      items ++ [%{label: "candidates", body: body}]
    end
  end

  defp add_action_candidates(items, _), do: items

  defp selected_reason(action_meta) when is_map(action_meta) do
    action_meta
    |> pget(:selected_candidate, %{})
    |> pget(:reason)
    |> maybe_format_frame_value()
  end

  defp selected_reason(_), do: nil

  defp maybe_inline_flag(items, _label, nil), do: items
  defp maybe_inline_flag(items, _label, false), do: items
  defp maybe_inline_flag(items, label, true), do: items ++ [label]

  defp maybe_inline_flag(items, label, value),
    do: items ++ ["#{label}=#{format_frame_value(value)}"]

  defp maybe_format_frame_value(nil), do: nil
  defp maybe_format_frame_value(value), do: format_frame_value(value)

  defp maybe_add_similar_terms_section(sections, bullets) when is_list(bullets) do
    sims =
      bullets
      |> Enum.filter(fn b -> is_binary(b[:similar]) and String.trim(b[:similar]) != "" end)
      |> Enum.map(fn b -> %{label: b.label, body: b.similar} end)

    if sims != [] do
      sections ++
        [
          %{
            key: :similar,
            title: "Similar terms",
            hint: "Related terms (synonyms / nearby words) surfaced for transparency.",
            items: sims
          }
        ]
    else
      sections
    end
  end

  defp maybe_add_antonyms_section(sections, bullets) when is_list(bullets) do
    ants =
      bullets
      |> Enum.filter(fn b -> is_binary(b[:antonyms]) and String.trim(b[:antonyms]) != "" end)
      |> Enum.map(fn b -> %{label: b.label, body: b.antonyms} end)

    if ants != [] do
      sections ++
        [
          %{
            key: :antonyms,
            title: "Opposites / antonyms",
            hint: "Opposing terms surfaced when available.",
            items: ants
          }
        ]
    else
      sections
    end
  end

  defp maybe_add_message_section(sections, main) when is_binary(main) do
    m = String.trim(main)

    if m != "" do
      sections ++
        [
          %{
            key: :message,
            title: "Message",
            hint: "Main assistant text (excluding tone/meta lines and lexical/debug tail).",
            items: [%{label: "content", body: m}]
          }
        ]
    else
      sections
    end
  end

  defp maybe_add_message_section(sections, _), do: sections

  defp maybe_kv(items, _k, nil), do: items
  defp maybe_kv(items, k, v), do: items ++ [%{label: k, body: to_string(v)}]

  defp maybe_raw(items, _label, nil, nil), do: items

  defp maybe_raw(items, label, raw, fallback) do
    v = raw || fallback

    if is_binary(v) and String.trim(v) != "" do
      items ++ [%{label: label, body: v}]
    else
      items
    end
  end

  defp sense_body(defn, ex) do
    d = defn
    e = ex

    cond do
      present?(d) and present?(e) ->
        "#{d}\nExample: #{e}"

      present?(d) ->
        d

      present?(e) ->
        "Example: #{e}"

      true ->
        "(definition unavailable)"
    end
  end

  defp present?(v) when is_binary(v), do: String.trim(v) != ""
  defp present?(_), do: false
end
