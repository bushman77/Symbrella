# apps/symbrella_web/lib/symbrella_web/live/home_live.ex
defmodule SymbrellaWeb.HomeLive do
  use SymbrellaWeb, :live_view

  alias SymbrellaWeb.ChatLive.HTML, as: ChatHTML
  alias Core.Response
  alias Core.LexicalExplain

  @choice_preview_limit 3
  @def_char_limit 120
  @senses_modal_limit 24

  # ─────────────────────────────────────────────────────────────────────────────
  # LiveView
  # ─────────────────────────────────────────────────────────────────────────────

  @impl true
  def mount(_params, _session, socket) do
    initial = [
      %{id: "m1", role: :assistant, text: "Welcome to Symbrella chat  👋"}
    ]

    initial_text_by_id = %{"m1" => "Welcome to Symbrella chat  👋"}

    initial_explain_by_id = %{
      "m1" => ChatHTML.explain_payload_for(%{id: "m1", text: "Welcome to Symbrella chat  👋"})
    }

    {:ok,
     socket
     |> assign(
       page_title: "Chat",
       draft: "",
       bot_typing: false,
       pending_task: nil,
       cancelled_ref: nil,
       session_id: "s-" <> Integer.to_string(System.unique_integer([:positive])),
       explain_open?: false,
       explain_payload: %{},
       message_text_by_id: initial_text_by_id,
       explain_by_id: initial_explain_by_id
     )
     |> stream(:messages, initial)}
  end

  @impl true
  def handle_event("update_draft", %{"message" => text}, socket) do
    {:noreply, assign(socket, :draft, to_string(text))}
  end

  @impl true
  def handle_event("send", %{"message" => raw}, socket) do
    text = sanitize_user_text(raw)

    cond do
      text == "" ->
        {:noreply, socket}

      socket.assigns.bot_typing ->
        {:noreply, socket}

      true ->
        msg_id = "u-" <> Integer.to_string(System.unique_integer([:positive]))

        msg = %{
          id: msg_id,
          role: :user,
          text: text
        }

        socket =
          socket
          |> stream_insert(:messages, msg)
          |> assign(
            draft: "",
            bot_typing: true,
            cancelled_ref: nil,
            message_text_by_id: Map.put(socket.assigns.message_text_by_id, msg_id, text)
          )
          |> push_event("chat:scroll", %{to: "composer"})

        task =
          Task.Supervisor.async_nolink(Symbrella.TaskSup, fn ->
            build_planned_reply(text)
          end)

        {:noreply, assign(socket, :pending_task, task)}
    end
  end

  @impl true
  def handle_event("explain_open", %{"id" => id} = params, socket) do
    text =
      case params do
        %{"text" => t} when is_binary(t) -> t
        _ -> Map.get(socket.assigns.message_text_by_id, id, "")
      end

    payload =
      socket.assigns.explain_by_id[id] ||
        ChatHTML.explain_payload_for(%{id: id, text: text})

    {:noreply, assign(socket, explain_open?: true, explain_payload: payload)}
  end

  @impl true
  def handle_event("explain_close", _params, socket) do
    {:noreply, assign(socket, explain_open?: false, explain_payload: %{})}
  end

  @impl true
  def handle_event("stop", _params, socket) do
    case socket.assigns.pending_task do
      %Task{} = task ->
        _ = Task.shutdown(task, :brutal_kill)
        Process.demonitor(task.ref, [:flush])

        bot_id = "x-" <> Integer.to_string(System.unique_integer([:positive]))
        bot_text = "(stopped)"

        payload = ChatHTML.explain_payload_for(%{id: bot_id, text: bot_text})

        {:noreply,
         socket
         |> assign(
           bot_typing: false,
           cancelled_ref: task.ref,
           pending_task: nil,
           message_text_by_id: Map.put(socket.assigns.message_text_by_id, bot_id, bot_text),
           explain_by_id: Map.put(socket.assigns.explain_by_id, bot_id, payload)
         )
         |> stream_insert(:messages, %{id: bot_id, role: :assistant, text: bot_text})
         |> push_event("chat:scroll", %{to: "composer"})}

      _ ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_info({ref, payload}, %{assigns: %{pending_task: %Task{ref: ref}}} = socket) do
    Process.demonitor(ref, [:flush])

    reply = to_bot_reply(payload)

    reply_text = reply.text
    tone = Map.get(reply, :tone)
    meta = Map.get(reply, :meta)
    si = Map.get(reply, :si, %{})

    explain_text = Map.get(reply, :explain_text, reply_text)
    senses_selected = Map.get(reply, :senses_selected, [])

    intent = Map.get(si, :intent)
    confidence = Map.get(si, :confidence)

    bot_id = "b-" <> Integer.to_string(System.unique_integer([:positive]))

    explain_payload =
      ChatHTML.explain_payload_for(%{
        id: bot_id,
        text: explain_text,
        tone: tone,
        intent: intent,
        confidence: confidence,
        from: meta || %{},
        senses_selected: senses_selected
      })

    bot = %{
      id: bot_id,
      role: :assistant,
      text: reply_text,
      tone: tone,
      meta: meta
    }

    {:noreply,
     socket
     |> assign(
       bot_typing: false,
       pending_task: nil,
       message_text_by_id: Map.put(socket.assigns.message_text_by_id, bot_id, explain_text),
       explain_by_id: Map.put(socket.assigns.explain_by_id, bot_id, explain_payload)
     )
     |> stream_insert(:messages, bot)
     |> push_event("chat:scroll", %{to: "composer"})}
  end

  @impl true
  def handle_info({ref, _payload}, socket) when is_reference(ref) do
    {:noreply, socket}
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, reason}, socket) do
    cond do
      socket.assigns.cancelled_ref == ref ->
        {:noreply, assign(socket, cancelled_ref: nil)}

      match?(%Task{ref: ^ref}, socket.assigns.pending_task) ->
        bot_id = "e-" <> Integer.to_string(System.unique_integer([:positive]))
        bot_text = "Oops—my brain hit a snag: #{Exception.format_exit(reason)}"

        payload = ChatHTML.explain_payload_for(%{id: bot_id, text: bot_text})

        msg = %{
          id: bot_id,
          role: :assistant,
          text: bot_text
        }

        {:noreply,
         socket
         |> assign(
           bot_typing: false,
           pending_task: nil,
           message_text_by_id: Map.put(socket.assigns.message_text_by_id, bot_id, bot_text),
           explain_by_id: Map.put(socket.assigns.explain_by_id, bot_id, payload)
         )
         |> stream_insert(:messages, msg)
         |> push_event("chat:scroll", %{to: "composer"})}

      true ->
        {:noreply, socket}
    end
  end

  @impl true
  def render(assigns), do: ChatHTML.chat(assigns)

  # ─────────────────────────────────────────────────────────────────────────────
  # Planner integration
  # ─────────────────────────────────────────────────────────────────────────────

  defp build_planned_reply(user_text) do
    si =
      Core.resolve_input(user_text,
        mode: :prod,
        enrich_lexicon?: true,
        lexicon_stage?: true
      )

    lexical_tail = build_lexical_tail(si)
    senses_selected = build_senses_selected(si)

    case Map.get(si, :response_text) do
      text when is_binary(text) and text != "" ->
        tone = Map.get(si, :response_tone, :warm)
        meta = Map.get(si, :response_meta, %{})

        visible = compose_tone_and_main(tone, text)

        explain_text =
          join_blocks([
            visible,
            lexical_tail,
            get_in(meta || %{}, [:explanation, :text])
          ])

        %{
          text: visible,
          tone: tone,
          meta: meta,
          si: si,
          senses_selected: senses_selected,
          explain_text: explain_text
        }

      _ ->
        si_like = %{
          intent: Map.get(si, :intent, :unknown),
          confidence: Map.get(si, :confidence, 0.0),
          keyword: Map.get(si, :keyword, ""),
          text: user_text,
          tokens: Map.get(si, :tokens, []),
          source: Map.get(si, :source, :user)
        }

        mood = mood_from_si(si)

        cond do
          Code.ensure_loaded?(Response) and function_exported?(Response, :plan, 2) ->
            {tone, reply_text, meta} = Response.plan(si_like, mood)

            visible = compose_tone_and_main(tone, reply_text)

            explain_text =
              join_blocks([
                visible,
                lexical_tail,
                get_in(meta || %{}, [:explanation, :text])
              ])

            %{
              text: visible,
              tone: tone,
              meta: meta,
              si: si,
              senses_selected: senses_selected,
              explain_text: explain_text
            }

          true ->
            debug_text = format_si_reply(si)
            visible = compose_tone_and_main(nil, debug_text)

            explain_text = join_blocks([visible, lexical_tail])

            %{
              text: visible,
              si: si,
              senses_selected: senses_selected,
              explain_text: explain_text
            }
        end
    end
  end

  defp build_lexical_tail(si) do
    tokens = Map.get(si, :tokens)
    token_structs = Map.get(si, :token_structs)

    token_count =
      cond do
        is_list(tokens) and tokens != [] -> length(tokens)
        is_list(token_structs) -> length(token_structs)
        true -> 0
      end

    max_items =
      cond do
        token_count <= 0 -> 3
        token_count <= 4 -> token_count
        true -> 3
      end

    if max_items > 0 and
         Code.ensure_loaded?(LexicalExplain) and
         function_exported?(LexicalExplain, :from_si, 2) do
      LexicalExplain.from_si(si, max_items: max_items)
    else
      ""
    end
  end

  defp mood_from_si(si) when is_map(si) do
    case Map.get(si, :mood) do
      %{} = m -> m
      _ -> %{}
    end
  end

  defp compose_tone_and_main(tone, main_text) do
    tone_line =
      case tone do
        t when is_atom(t) -> Atom.to_string(t)
        t when is_binary(t) -> t
        _ -> nil
      end

    [tone_line, main_text]
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.join("\n")
  end

  defp join_blocks(blocks) when is_list(blocks) do
    blocks
    |> Enum.map(fn
      nil -> ""
      v -> to_string(v)
    end)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join("\n\n")
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Senses Selected builder (NO Db calls; respects db <- brain <- core <- web)
  # ─────────────────────────────────────────────────────────────────────────────

  defp build_senses_selected(%{} = si) do
    choices = Map.get(si, :lifg_choices, []) || []
    cells = si_cells(si)

    {items, _seen} =
      Enum.reduce(choices, {[], MapSet.new()}, fn ch, {acc, seen} ->
        tok_idx = ch[:token_index] || ch[:tok_index] || ch[:index]

        id = ch[:id] || ch[:chosen_id]
        pos = ch[:pos] || ch[:chosen_pos]
        src = ch[:src] || ch[:source]
        score = ch[:score]

        lemma = ch[:lemma] || ch[:token] || ch[:surface] || ""

        surface =
          ch[:surface] ||
            ch[:raw_phrase] ||
            ch[:token_phrase] ||
            lemma

        alt_ids = List.wrap(ch[:alt_ids])
        key = {tok_idx, id, pos, src, surface, lemma}

        if MapSet.member?(seen, key) do
          {acc, seen}
        else
          label =
            [
              safe_str(surface),
              " → ",
              safe_str(lemma),
              " pos=",
              safe_str(pos),
              " src=",
              safe_str(src),
              " score=",
              fmt_score(score)
            ]
            |> IO.iodata_to_binary()
            |> String.trim()

          defn =
            first_present([
              definition_for_choice(ch, cells),
              candidate_definition_from_si(si, ch)
            ])

          ex =
            first_present([
              example_for_choice(ch, cells),
              candidate_example_from_si(si, ch)
            ])

          item = %{
            token_index: tok_idx,
            id: id,
            alt_ids: alt_ids,
            label: label,
            definition: defn,
            example: ex
          }

          {[item | acc], MapSet.put(seen, key)}
        end
      end)

    items
    |> Enum.reverse()
    |> Enum.take(@senses_modal_limit)
  end

  defp build_senses_selected(_), do: []

  defp si_cells(si) do
    Map.get(si, :active_cells, Map.get(si, :cells, [])) || []
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Candidate field lookups from si.sense_candidates
  # ─────────────────────────────────────────────────────────────────────────────

  defp candidate_definition_from_si(%{} = si, choice),
    do: candidate_field_from_si(si, choice, [:definition, :def, :gloss, :meaning])

  defp candidate_definition_from_si(_, _), do: ""

  defp candidate_example_from_si(%{} = si, choice),
    do: candidate_field_from_si(si, choice, [:example, :ex, :usage, :sample])

  defp candidate_example_from_si(_, _), do: ""

  defp candidate_field_from_si(%{} = si, choice, keys) when is_list(keys) do
    sc = Map.get(si, :sense_candidates) || Map.get(si, "sense_candidates") || %{}

    id = choice[:id] || choice[:chosen_id]
    tok_idx = choice[:token_index] || choice[:tok_index] || choice[:index]

    by_tok =
      case tok_idx do
        nil ->
          []

        i ->
          Map.get(sc, i) ||
            Map.get(sc, Integer.to_string(i)) ||
            Map.get(sc, to_string(i)) ||
            []
      end

    all_lists =
      if is_list(by_tok) and by_tok != [] do
        [by_tok]
      else
        sc
        |> Enum.flat_map(fn
          {_k, v} when is_list(v) -> [v]
          _ -> []
        end)
      end

    cand =
      all_lists
      |> Enum.flat_map(fn v -> if is_list(v), do: v, else: [] end)
      |> Enum.find(fn c ->
        cand_id = cand_get(c, :id) || cand_get(c, :chosen_id)
        cand_id == id
      end)

    keys
    |> Enum.find_value(fn k -> cand_get(cand, k) end)
    |> Kernel.||("")
    |> to_string()
    |> gloss()
  end

  defp cand_get(map, key) when is_map(map) and is_atom(key) do
    Map.get(map, key) || Map.get(map, Atom.to_string(key))
  end

  defp cand_get(map, key) when is_map(map), do: Map.get(map, key)
  defp cand_get(_, _), do: nil

  # ─────────────────────────────────────────────────────────────────────────────
  # Definition/example resolution (cells + choice + alt_ids)
  # ─────────────────────────────────────────────────────────────────────────────

  defp definition_for_choice(choice, cells) do
    by_id = index_cells_by_id(cells)
    by_norm = index_cells_by_norm(cells)

    id = choice[:id] || choice[:chosen_id]
    alt_ids = List.wrap(choice[:alt_ids])
    lemma_norm = norm_text(choice[:lemma] || choice[:token] || "")

    direct =
      choice[:definition] ||
        choice[:def] ||
        choice[:gloss] ||
        choice[:meaning]

    with_direct = gloss(direct)

    with_choice =
      by_id
      |> Map.get(id)
      |> cell_def()
      |> gloss()

    cond do
      with_direct != "" ->
        with_direct

      with_choice != "" ->
        with_choice

      true ->
        with_alt = first_def_from_ids(alt_ids, by_id)

        cond do
          with_alt != "" ->
            with_alt

          true ->
            idn = id_norm(id)

            first_present([
              by_norm |> Map.get(idn) |> cell_def() |> gloss(),
              by_norm |> Map.get(lemma_norm) |> cell_def() |> gloss()
            ])
        end
    end
  end

  defp example_for_choice(choice, cells) do
    by_id = index_cells_by_id(cells)
    by_norm = index_cells_by_norm(cells)

    id = choice[:id] || choice[:chosen_id]
    alt_ids = List.wrap(choice[:alt_ids])
    lemma_norm = norm_text(choice[:lemma] || choice[:token] || "")

    direct =
      choice[:example] ||
        choice[:ex] ||
        choice[:usage] ||
        choice[:sample]

    with_direct = gloss(direct)

    with_choice =
      by_id
      |> Map.get(id)
      |> cell_ex()
      |> gloss()

    cond do
      with_direct != "" ->
        with_direct

      with_choice != "" ->
        with_choice

      true ->
        with_alt = first_ex_from_ids(alt_ids, by_id)

        cond do
          with_alt != "" ->
            with_alt

          true ->
            idn = id_norm(id)

            first_present([
              by_norm |> Map.get(idn) |> cell_ex() |> gloss(),
              by_norm |> Map.get(lemma_norm) |> cell_ex() |> gloss()
            ])
        end
    end
  end

  defp first_def_from_ids([], _by_id), do: ""

  defp first_def_from_ids([h | t], by_id) do
    case by_id |> Map.get(h) |> cell_def() |> gloss() do
      "" -> first_def_from_ids(t, by_id)
      d -> d
    end
  end

  defp first_ex_from_ids([], _by_id), do: ""

  defp first_ex_from_ids([h | t], by_id) do
    case by_id |> Map.get(h) |> cell_ex() |> gloss() do
      "" -> first_ex_from_ids(t, by_id)
      e -> e
    end
  end

  defp index_cells_by_id(cells) do
    Enum.reduce(cells || [], %{}, fn c, acc ->
      case cell_id(c) do
        nil -> acc
        id -> Map.put(acc, id, c)
      end
    end)
  end

  defp index_cells_by_norm(cells) do
    Enum.reduce(cells || [], %{}, fn c, acc ->
      case cell_norm(c) do
        nil -> acc
        n -> Map.put_new(acc, n, c)
      end
    end)
  end

  defp cell_id(c) when is_map(c) do
    case Map.get(c, :id) || Map.get(c, "id") do
      id when is_binary(id) -> id
      _ -> nil
    end
  end

  defp cell_id(_), do: nil

  defp cell_norm(c) when is_map(c) do
    cond do
      is_binary(Map.get(c, :norm)) -> Map.get(c, :norm)
      is_binary(Map.get(c, "norm")) -> Map.get(c, "norm")
      is_binary(Map.get(c, :id)) -> id_norm(Map.get(c, :id))
      is_binary(Map.get(c, "id")) -> id_norm(Map.get(c, "id"))
      true -> nil
    end
  end

  defp cell_norm(_), do: nil

  defp id_norm(nil), do: nil
  defp id_norm(id) when is_binary(id), do: id |> String.split("|") |> List.first()

  defp cell_def(c) when is_map(c) do
    Map.get(c, :definition) ||
      Map.get(c, "definition") ||
      Map.get(c, :def) ||
      Map.get(c, "def") ||
      Map.get(c, :gloss) ||
      Map.get(c, "gloss")
  end

  defp cell_def(_), do: nil

  defp cell_ex(c) when is_map(c) do
    Map.get(c, :example) ||
      Map.get(c, "example") ||
      Map.get(c, :ex) ||
      Map.get(c, "ex") ||
      Map.get(c, :usage) ||
      Map.get(c, "usage") ||
      Map.get(c, :sample) ||
      Map.get(c, "sample")
  end

  defp cell_ex(_), do: nil

  # ─────────────────────────────────────────────────────────────────────────────
  # Formatting / debug
  # ─────────────────────────────────────────────────────────────────────────────

  defp format_si_reply(si) do
    tokens = Map.get(si, :tokens, [])
    cells = si_cells(si)
    choices = Map.get(si, :lifg_choices, [])

    token_preview =
      tokens
      |> Enum.take(6)
      |> Enum.map(fn
        %{phrase: p} -> p
        p when is_binary(p) -> p
        _ -> "<?>"
      end)
      |> Enum.join(", ")

    choices_preview =
      choices
      |> Enum.take(@choice_preview_limit)
      |> Enum.map(&format_choice_with_def(&1, cells))
      |> Enum.reject(&(&1 in [nil, ""]))
      |> Enum.join(", ")

    base =
      "Got it. tokens=#{length(tokens)}, cells=#{length(cells)}, lifg=#{length(choices)}" <>
        " · src=#{inspect(Map.get(si, :source))}" <>
        if(token_preview != "", do: " · [#{token_preview}]", else: "")

    if choices_preview == "" do
      base
    else
      base <> " · choices: [#{choices_preview}]"
    end
  end

  defp format_choice_with_def(choice, cells) do
    lemma = choice[:lemma] || choice[:token] || ""
    id = choice[:id] || choice[:chosen_id]
    score = fmt_score(choice[:score])

    alt =
      case choice[:alt_ids] do
        [a | _] -> " ; alt " <> short_id(a)
        _ -> ""
      end

    defn = definition_for_choice(choice, cells)
    head = "#{lemma}→#{short_id(id)}(#{score}#{alt})"
    if defn == "", do: head, else: head <> " — " <> defn
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # General utilities
  # ─────────────────────────────────────────────────────────────────────────────

  defp sanitize_user_text(text) do
    text
    |> to_string()
    |> String.replace(~r/\r\n?/, "\n")
    |> String.replace(~r/[ \t]+(\n)/, "\\1")
    |> String.replace(~r/\n{3,}/, "\n\n")
    |> String.trim()
  end

  defp to_bot_reply({_, %{text: _} = reply}), do: reply
  defp to_bot_reply(%{text: _} = reply), do: reply
  defp to_bot_reply(text) when is_binary(text), do: %{text: text}
  defp to_bot_reply(other), do: %{text: inspect(other)}

  defp safe_str(nil), do: ""
  defp safe_str(v) when is_atom(v), do: Atom.to_string(v)
  defp safe_str(v) when is_binary(v), do: v
  defp safe_str(v), do: to_string(v)

  defp gloss(nil), do: ""
  defp gloss(""), do: ""

  defp gloss(str) when is_binary(str) do
    s = str |> String.replace(~r/\s+/u, " ") |> String.trim()

    if String.length(s) <= @def_char_limit do
      s
    else
      String.slice(s, 0, @def_char_limit) <> "…"
    end
  end

  defp gloss(other), do: other |> to_string() |> gloss()

  defp short_id(nil), do: "∅"
  defp short_id(id) when is_binary(id), do: id |> String.split("|") |> hd()

  defp fmt_score(nil), do: "—"
  defp fmt_score(s) when is_integer(s), do: Integer.to_string(s)
  defp fmt_score(s) when is_float(s), do: :erlang.float_to_binary(s, decimals: 2)

  defp fmt_score(s) when is_binary(s) do
    case Float.parse(String.trim(s)) do
      {f, _} -> :erlang.float_to_binary(f, decimals: 2)
      _ -> s
    end
  end

  defp fmt_score(s), do: to_string(s)

  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp norm_text(v),
    do: v |> to_string() |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp first_present(list) when is_list(list) do
    list
    |> Enum.find("", fn v -> is_binary(v) and String.trim(v) != "" end)
  end
end
