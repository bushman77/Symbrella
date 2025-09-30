defmodule SymbrellaWeb.HomeLive do
  use SymbrellaWeb, :live_view

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
  defp format_si_reply(si) do
    tokens  = Map.get(si, :tokens, [])
    cells   = Map.get(si, :active_cells, Map.get(si, :cells, []))
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
        if token_preview != "", do: " · [#{token_preview}]", else: ""

    if choices_preview == "" do
      base
    else
      base <> " · choices: [#{choices_preview}]"
    end
  end

  # Show winner + score; append a definition with smart fallbacks.
  defp format_choice_with_def(choice, cells) do
    lemma = choice[:lemma] || ""
    id    = choice[:id]
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
    by_id   = index_cells_by_id(cells)
    by_norm = index_cells_by_norm(cells)

    id         = choice[:id]
    alt_ids    = List.wrap(choice[:alt_ids])
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
                  # FINAL FALLBACK: ask the lexicon live by lemma
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
      d  -> d
    end
  end

  defp index_cells_by_id(cells) do
    Enum.reduce(cells || [], %{}, fn c, acc ->
      case cell_id(c) do
        nil -> acc
        id  -> Map.put(acc, id, c)
      end
    end)
  end

  defp index_cells_by_norm(cells) do
    Enum.reduce(cells || [], %{}, fn c, acc ->
      case cell_norm(c) do
        nil -> acc
        n   -> Map.put_new(acc, n, c)  # keep first with that norm
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
    if String.length(s) <= @def_char_limit, do: s, else: String.slice(s, 0, @def_char_limit) <> "…"
  end

  defp short_id(nil), do: "∅"
  defp short_id(id) when is_binary(id), do: id |> String.split("|") |> hd()

  defp fmt_score(nil), do: "—"
  defp fmt_score(s) when is_number(s), do: :erlang.float_to_binary(s, decimals: 2)

  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()
  defp norm_text(v),
    do:
      v
      |> Kernel.to_string()
      |> String.downcase()
      |> String.replace(~r/\s+/u, " ")
      |> String.trim()

  # Final fallback: query lexicon by lemma
  defp lexicon_def(word) when is_binary(word) and word != "" do
    try do
      case Core.Lexicon.lookup(word) do
        %{senses: [s | _]} ->
          (s[:definition] || s[:def] || "") |> gloss()
        _ ->
          ""
      end
    rescue
      _ -> ""
    catch
      _, _ -> ""
    end
  end
  defp lexicon_def(_), do: ""

  # ---------- liveview ----------
  @impl true
  def mount(_params, _session, socket) do
    initial = [
      %{id: "m1", role: :assistant, text: "Welcome to Symbrella chat  👋"}
    ]

    {:ok,
     socket
     |> assign(
       page_title: "Chat",
       draft: "",
       bot_typing: false,
       pending_task: nil,
       cancelled_ref: nil,
       session_id: "s-" <> Integer.to_string(System.unique_integer([:positive]))
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
      msg = %{
        id: "u-" <> Integer.to_string(System.unique_integer([:positive])),
        role: :user,
        text: text
      }

      socket =
        socket
        |> stream_insert(:messages, msg)
        |> assign(draft: "", bot_typing: true, cancelled_ref: nil)
        |> push_event("chat:scroll", %{to: "composer"})

      # Run prod resolve (Core handles enrichment, re-query, LIFG, and Brain activation)
      task =
        Task.Supervisor.async_nolink(Symbrella.TaskSup, fn ->
          opts =
            Application.get_env(
              :symbrella,
              :resolve_input_opts,
              [mode: :prod, enrich_lexicon?: true, lexicon_stage?: true]
            )

          si = Core.resolve_input(text, opts)

          # Return a small, friendly UI summary
          %{text: format_si_reply(si)}
        end)

      {:noreply, assign(socket, :pending_task, task)}
    end
  end

  # 🛑 User clicked Stop
  @impl true
  def handle_event("stop", _params, socket) do
    case socket.assigns.pending_task do
      %Task{ref: ref} = task ->
        _ = Task.shutdown(task, :brutal_kill)

        {:noreply,
         socket
         |> assign(bot_typing: false, cancelled_ref: ref, pending_task: nil)
         |> stream_insert(:messages, %{
           id: "x-" <> Integer.to_string(System.unique_integer([:positive])),
           role: :assistant,
           text: "(stopped)"
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

    %{text: reply_text} = to_bot_reply(payload)

    bot = %{
      id: "b-" <> Integer.to_string(System.unique_integer([:positive])),
      role: :assistant,
      text: reply_text
    }

    {:noreply,
     socket
     |> assign(bot_typing: false, pending_task: nil)
     |> stream_insert(:messages, bot)
     |> push_event("chat:scroll", %{to: "composer"})}
  end

  # ❌ Task exited — distinguish user cancel vs real error
  @impl true
  def handle_info({:DOWN, ref, :process, _pid, reason}, socket) do
    cond do
      socket.assigns.cancelled_ref == ref ->
        {:noreply, assign(socket, cancelled_ref: nil)}

      match?(%Task{ref: ^ref}, socket.assigns.pending_task) ->
        msg = %{
          id: "e-" <> Integer.to_string(System.unique_integer([:positive])),
          role: :assistant,
          text: "Oops—my brain hit a snag: #{Exception.format_exit(reason)}"
        }

        {:noreply,
         socket
         |> assign(bot_typing: false, pending_task: nil)
         |> stream_insert(:messages, msg)
         |> push_event("chat:scroll", %{to: "composer"})}

      true ->
        {:noreply, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id="chat-root"
      class="relative h-[100svh] min-h-[100svh] bg-[var(--color-bg)] text-[var(--color-text)]"
    >
      <!-- FIXED HEADER -->
      <header
        id="chat-header"
        phx-hook="HeaderSizer"
        class="fixed top-0 left-0 right-0 z-20 border-b border-slate-800/60 bg-[var(--color-bg)]/90 backdrop-blur"
      >
        <div class="mx-auto max-w-4xl w-full px-4 py-3 flex items-center justify-between">
          <h1 class="text-base sm:text-lg font-semibold">Symbrella · Chat</h1>
          <div class="text-xs opacity-70 hidden sm:block">LiveView</div>
        </div>
      </header>

      <!-- MESSAGES -->
      <main
        id="messages"
        phx-hook="ScrollOnEvent"
        class="absolute left-0 right-0 overflow-y-auto scroll-smooth"
        style="top: var(--hdr,56px); bottom: var(--ftr,72px);"
      >
        <div class="mx-auto max-w-4xl w-full px-3 sm:px-4 py-4">
          <div id="message-list" phx-update="stream" class="space-y-3">
            <%= for {dom_id, m} <- @streams.messages do %>
              <div
                id={dom_id}
                class={if m.role == :user, do: "flex justify-end", else: "flex justify-start"}
              >
                <div class={
                  if m.role == :user do
                    "max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-sky-600/80 text-slate-50 shadow"
                  else
                    "max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-[var(--color-panel)] border border-slate-800/60 shadow"
                  end
                }>
                  <p class="whitespace-pre-wrap">{m.text}</p>
                </div>
              </div>
            <% end %>
          </div>

          <%= if @bot_typing do %>
            <div id="typing" class="mt-3 flex justify-start">
              <div class="max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-[var(--color-panel)] border border-slate-800/60 shadow">
                <span class="opacity-70">Symbrella is thinking…</span>
              </div>
            </div>
          <% end %>

          <div id="bottom"></div>
        </div>
      </main>

      <!-- COMPOSER -->
      <footer
        id="chat-composer"
        phx-hook="FooterSizer"
        class="fixed bottom-0 left-0 right-0 z-30 border-t border-slate-800/60 bg-[var(--color-bg)]/95 backdrop-blur"
      >
        <div class="mx-auto max-w-4xl w-full px-3 sm:px-4 pt-2 pb-3">
          <form phx-submit="send" phx-change="update_draft" class="flex items-end gap-2">
            <textarea
              id="chat-input"
              name="message"
              phx-hook="ChatInput"
              phx-debounce="200"
              rows="1"
              placeholder="Type a message… (Enter to send, Shift+Enter for newline)"
              class="flex-1 resize-none rounded-2xl border border-slate-800/60 bg-[var(--color-panel)] px-4 py-3 text-[15px] outline-none focus:ring-2 focus:ring-[var(--color-accent)]/40"
              disabled={@bot_typing}
              aria-busy={@bot_typing}
            ><%= @draft %></textarea>
            <%= if @bot_typing do %>
              <button type="button" phx-click="stop" class="btn px-4 py-3 rounded-2xl shadow">
                🛑 Stop
              </button>
            <% else %>
              <button type="submit" class="btn px-4 py-3 rounded-2xl shadow">Send</button>
            <% end %>
          </form>
        </div>
      </footer>
    </div>
    """
  end
end

