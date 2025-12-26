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

    bullets =
      extra_wo_meta
      |> parse_reading_bullets()
      |> then(fn bs -> if bs == [], do: parse_reading_bullets(text), else: bs end)

    sections =
      []
      |> maybe_add_intent_tone_section(meta, meta_line)
      |> maybe_add_senses_selected_section(structured_senses, bullets)
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
          senses_selected: pget(payload, :senses_selected)
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
      intent: message[:intent] || from[:intent] || parsed[:intent],
      confidence: message[:confidence] || from[:confidence] || parsed[:confidence],
      tone: message[:tone] || from[:tone_reaction] || parsed[:tone],
      because: message[:tone_because] || from[:source_latents] || parsed[:because],
      mode: message[:mode] || parsed[:mode],
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
      {f, _} -> f
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
        meta_line

    if has_any? do
      items =
        []
        |> maybe_kv("intent", meta[:intent] && ":#{meta[:intent]}")
        |> maybe_kv("confidence", meta[:confidence] && format_conf(meta[:confidence]))
        |> maybe_kv("tone", meta[:tone] && ":#{meta[:tone]}")
        |> maybe_kv("because", meta[:because])
        |> maybe_kv("mode", meta[:mode])
        |> maybe_raw("raw line", meta[:raw], meta_line)

      sections ++
        [
          %{
            key: :intent_tone,
            title: "Intent & tone",
            hint: "Classifier + tone selection metadata that shaped the reply.",
            items: items
          }
        ]
    else
      sections
    end
  end

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
