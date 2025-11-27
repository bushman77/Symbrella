defmodule SymbrellaWeb.HomeLive do
  use SymbrellaWeb, :live_view

  alias SymbrellaWeb.ChatLive.HTML, as: ChatHTML
  alias Core.Response
  alias Core.LexicalExplain

  @choice_preview_limit 3
  @def_char_limit 120

  # ---------- helpers ----------

  defp sanitize_user_text(text) do
    text
    |> to_string()
    # CRLF -> LF
    |> String.replace(~r/\r\n?/, "\n")
    # strip trailing spaces
    |> String.replace(~r/[ \t]+(\n)/, "\\1")
    # collapse 3+ newlines
    |> String.replace(~r/\n{3,}/, "\n\n")
    |> String.trim()
  end

  defp to_bot_reply({_, %{text: _} = reply}), do: reply
  defp to_bot_reply(%{text: _} = reply), do: reply
  defp to_bot_reply(text) when is_binary(text), do: %{text: text}
  defp to_bot_reply(other), do: %{text: inspect(other)}

  # A tiny formatter so the UI shows something friendly when SI comes back.
  # (kept as a fallback if the planner module isn't present)
  defp format_si_reply(si) do
    tokens = Map.get(si, :tokens, [])
    cells = Map.get(si, :active_cells, Map.get(si, :cells, []))
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

  # Show winner + score; append a definition with smart fallbacks.
  defp format_choice_with_def(choice, cells) do
    lemma = choice[:lemma] || ""
    id = choice[:id]
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

  defp definition_for_choice(choice, cells) do
    by_id = index_cells_by_id(cells)
    by_norm = index_cells_by_norm(cells)

    id = choice[:id]
    alt_ids = List.wrap(choice[:alt_ids])
    lemma_norm = norm_text(choice[:lemma] || "")

    # 1) chosen id
    with_choice =
      by_id
      |> Map.get(id)
      |> cell_def()
      |> gloss()

    cond do
      with_choice != "" ->
        with_choice

      true ->
        # 2) first alt id with a def
        with_alt = first_def_from_ids(alt_ids, by_id)

        cond do
          with_alt != "" ->
            with_alt

          true ->
            # 3a) same norm as chosen id (parsed from id)
            idn = id_norm(id)

            with_same_norm =
              by_norm
              |> Map.get(idn)
              |> cell_def()
              |> gloss()

            cond do
              with_same_norm != "" ->
                with_same_norm

              true ->
                # 3b) any cell matching lemma norm
                with_lemma =
                  by_norm
                  |> Map.get(lemma_norm)
                  |> cell_def()
                  |> gloss()

                if with_lemma != "" do
                  with_lemma
                else
                  # FINAL FALLBACK: remote lexicon disabled → no external lookup
                  lexicon_def(lemma_norm)
                end
            end
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
        # keep first with that norm
        n -> Map.put_new(acc, n, c)
      end
    end)
  end

  # NOTE: Use Map.get to support structs (Ecto schemas) and plain maps.
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

  defp cell_def(c) when is_map(c),
    do: Map.get(c, :definition) || Map.get(c, "definition")

  defp cell_def(_), do: nil

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

  defp short_id(nil), do: "∅"
  defp short_id(id) when is_binary(id), do: id |> String.split("|") |> hd()

  defp fmt_score(nil), do: "—"
  defp fmt_score(s) when is_number(s), do: :erlang.float_to_binary(s, decimals: 2)

  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp norm_text(v) do
    v
    |> Kernel.to_string()
    |> String.downcase()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  # Final fallback (remote OFF): don't query anything; show no extra gloss.
  defp lexicon_def(_), do: ""

  # ---------- Planner integration ----------

  # Compose:
  #   tone line
  #   + main text
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

  # Build a lexical "tail" string (By the way...) from SI, for modal use.
  defp build_lexical_tail(si) do
    token_count = si |> Map.get(:tokens, []) |> length()

    max_items =
      cond do
        token_count <= 0 -> 0
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

  # Prefer mood from this run’s SI (prevents “last-turn mood bleed”).
  defp mood_from_si(si) when is_map(si) do
    case Map.get(si, :mood) do
      %{} = m -> m
      _ -> %{}
    end
  end

  # Build the visible text + an internal modal "source" that includes
  # lexical tail + intent/tone explanation lines.
  #
  # IMPORTANT: do NOT pull intent from Brain.latest_intent/0 here.
  # Always derive from the *current* SI to avoid cross-turn bleed.
  defp build_planned_reply(user_text) do
    # Run the full golden pipeline
    si =
      Core.resolve_input(user_text,
        mode: :prod,
        enrich_lexicon?: true,
        lexicon_stage?: true
      )

    lexical_tail = build_lexical_tail(si)

    # 1️⃣ Preferred path: pipeline already attached response fields
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
          explain_text: explain_text
        }

      _ ->
        # 2️⃣ Fallback: use the planner directly (legacy path)
        # Derive intent/confidence/keyword from THIS si (not global last intent).
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
              explain_text: explain_text
            }

          true ->
            # 3️⃣ Final fallback: SI debug string so we never go silent
            debug_text = format_si_reply(si)
            visible = compose_tone_and_main(nil, debug_text)

            explain_text =
              join_blocks([
                visible,
                lexical_tail
              ])

            %{
              text: visible,
              si: si,
              explain_text: explain_text
            }
        end
    end
  end

  # ---------- liveview ----------

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
       # Explain modal state
       explain_open?: false,
       explain_payload: %{},
       # Local lookup so Explain works even if button only sends "id"
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

    # Block empty sends or if a turn is already running
    if text == "" or socket.assigns.bot_typing do
      {:noreply, socket}
    else
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

  # 🔍 Explain modal open
  @impl true
  def handle_event("explain_open", %{"id" => id} = params, socket) do
    # If the button was updated to also send text, use it. Otherwise use our local maps.
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

  # ❎ Explain modal close
  @impl true
  def handle_event("explain_close", _params, socket) do
    {:noreply, assign(socket, explain_open?: false, explain_payload: %{})}
  end

  # 🛑 User clicked Stop
  @impl true
  def handle_event("stop", _params, socket) do
    case socket.assigns.pending_task do
      %Task{} = task ->
        # Mark this ref as cancelled so :DOWN doesn’t create an error bubble.
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
         |> stream_insert(:messages, %{
           id: bot_id,
           role: :assistant,
           text: bot_text
         })
         |> push_event("chat:scroll", %{to: "composer"})}

      _ ->
        {:noreply, socket}
    end
  end

  # ✅ Task succeeded — accept map, text, or tuple payloads
  @impl true
  def handle_info({ref, payload}, %{assigns: %{pending_task: %Task{ref: ref}}} = socket) do
    Process.demonitor(ref, [:flush])

    reply = to_bot_reply(payload)
    reply_text = reply.text
    tone = Map.get(reply, :tone)
    meta = Map.get(reply, :meta)
    explain_text = Map.get(reply, :explain_text, reply_text)

    bot_id = "b-" <> Integer.to_string(System.unique_integer([:positive]))

    explain_payload = ChatHTML.explain_payload_for(%{id: bot_id, text: explain_text})

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
       # Store the full explain text for safety; modal can use this even without cached payload.
       message_text_by_id: Map.put(socket.assigns.message_text_by_id, bot_id, explain_text),
       explain_by_id: Map.put(socket.assigns.explain_by_id, bot_id, explain_payload)
     )
     |> stream_insert(:messages, bot)
     |> push_event("chat:scroll", %{to: "composer"})}
  end

  # 🧹 Ignore stale Task result messages (e.g. if a Task died after stop).
  @impl true
  def handle_info({ref, _payload}, socket) when is_reference(ref) do
    {:noreply, socket}
  end

  # ❌ Task exited — distinguish user cancel vs real error
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
end

