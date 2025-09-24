defmodule SymbrellaWeb.HomeLive do
  use SymbrellaWeb, :live_view

  # ---------- helpers ----------
  defp sanitize_user_text(text) do
    text
    |> to_string()
    |> String.replace(~r/\r\n?/, "\n")     # CRLF -> LF
    |> String.replace(~r/[ \t]+(\n)/, "\\1") # strip trailing spaces on lines
    |> String.replace(~r/\n{3,}/, "\n\n")  # collapse 3+ newlines
    |> String.trim()                       # trim both ends
  end

  defp to_bot_reply({_, %{text: _} = reply}), do: reply
  defp to_bot_reply(%{text: _} = reply),     do: reply
  defp to_bot_reply(text) when is_binary(text), do: %{text: text}
  defp to_bot_reply(other), do: %{text: inspect(other)}

  # ---------- liveview ----------
  @impl true
  def mount(_params, _session, socket) do
    initial = [
      %{id: "m1", role: :assistant, text: "Welcome to Symbrella chat 👋"}
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

      task =
        Task.Supervisor.async_nolink(Symbrella.TaskSup, fn ->
          Core.resolve_input(text)
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
    <div id="chat-root" class="relative h-[100svh] min-h-[100svh] bg-[var(--color-bg)] text-[var(--color-text)]">
      <!-- FIXED HEADER -->
      <header id="chat-header" phx-hook="HeaderSizer"
              class="fixed top-0 left-0 right-0 z-20 border-b border-slate-800/60 bg-[var(--color-bg)]/90 backdrop-blur">
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
        style={"top: var(--hdr,56px); bottom: var(--ftr,72px);"}
      >
        <div class="mx-auto max-w-4xl w-full px-3 sm:px-4 py-4">
          <div id="message-list" phx-update="stream" class="space-y-3">
            <%= for {dom_id, m} <- @streams.messages do %>
              <div id={dom_id} class={if m.role == :user, do: "flex justify-end", else: "flex justify-start"}>
                <div class={
                  if m.role == :user do
                    "max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-sky-600/80 text-slate-50 shadow"
                  else
                    "max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-[var(--color-panel)] border border-slate-800/60 shadow"
                  end
                }>
                  <p class="whitespace-pre-wrap"><%= m.text %></p>
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
      <footer id="chat-composer" phx-hook="FooterSizer"
              class="fixed bottom-0 left-0 right-0 z-30 border-t border-slate-800/60 bg-[var(--color-bg)]/95 backdrop-blur">
        <div class="mx-auto max-w-4xl w-full px-3 sm:px-4 pt-2 pb-3">
          <form phx-submit="send" phx-change="update_draft" class="flex items-end gap-2">
            <textarea id="chat-input"
                      name="message"
                      phx-hook="ChatInput"
                      phx-debounce="200"
                      rows="1"
                      placeholder="Type a message… (Enter to send, Shift+Enter for newline)"
                      class="flex-1 resize-none rounded-2xl border border-slate-800/60 bg-[var(--color-panel)] px-4 py-3 text-[15px] outline-none focus:ring-2 focus:ring-[var(--color-accent)]/40"
                      disabled={@bot_typing}
                      aria-busy={@bot_typing}><%= @draft %></textarea>
            <%= if @bot_typing do %>
              <button type="button" phx-click="stop" class="btn px-4 py-3 rounded-2xl shadow">🛑 Stop</button>
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

