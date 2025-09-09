defmodule SymbrellaWeb.HomeLive do
  use SymbrellaWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    initial =
      [
        %{id: "m1", role: :assistant, text: "Welcome to Symbrella chat 👋"}
      ]

    {:ok,
     socket
     |> assign(page_title: "Chat", draft: "", bot_typing: false)
     |> stream(:messages, initial)}
  end

  @impl true
  def handle_event("update_draft", %{"message" => text}, socket) do
    {:noreply, assign(socket, :draft, text)}
  end

  @impl true
  def handle_event("send", %{"message" => text}, socket) do
    text =
      text
      |> to_string()
      |> String.trim_trailing()                         # keep leading, trim trailing
      |> String.replace(~r/(?:\r\n|\r|\n){3,}/, "\n\n") # compress >2 newlines

    if String.trim(text) == "" do
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
        |> assign(draft: "", bot_typing: true)
        |> push_event("chat:scroll", %{})

      # Simulate async compute; swap with your real pipeline message when ready
      Process.send_after(self(), {:bot_reply, text}, 350)

      {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:bot_reply, user_text}, socket) do
    bot = %{
      id: "b-" <> Integer.to_string(System.unique_integer([:positive])),
      role: :assistant,
      text: "You said: " <> user_text
    }

    {:noreply,
     socket
     |> stream_insert(:messages, bot)
     |> assign(:bot_typing, false)
     |> push_event("chat:scroll", %{})}
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

      <!-- MESSAGES: fills between header & footer -->
      <main
        id="messages"
        phx-hook="AutoScroll"
        class="absolute left-0 right-0 overflow-y-auto scroll-smooth"
        style={"top: var(--hdr,56px); bottom: var(--ftr,72px);"}
      >
        <div class="mx-auto max-w-4xl w-full px-3 sm:px-4 py-4">
          <!-- Streams replace deprecated phx-update='append' -->
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

          <!-- Typing indicator lives OUTSIDE the stream container -->
          <%= if @bot_typing do %>
            <div id="typing" class="mt-3 flex justify-start">
              <div class="max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-[var(--color-panel)] border border-slate-800/60 shadow">
                <span class="opacity-70">Symbrella is typing…</span>
              </div>
            </div>
          <% end %>

          <!-- Anchor for scroll -->
          <div id="bottom"></div>
        </div>
      </main>

      <!-- FIXED FOOTER (COMPOSER) -->
      <footer id="chat-composer" phx-hook="FooterSizer"
              class="fixed bottom-0 left-0 right-0 z-30 border-t border-slate-800/60 bg-[var(--color-bg)]/95 backdrop-blur">
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
            ><%= @draft %></textarea>

            <button type="submit" class="btn px-4 py-3 rounded-2xl shadow">Send</button>
          </form>
        </div>
      </footer>
    </div>
    """
  end
end

