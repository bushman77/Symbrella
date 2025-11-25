defmodule SymbrellaWeb.ChatLive.HTML do
  @moduledoc """
  Presentation for the chat LiveView (HomeLive). Pure HEEx & helpers only.
  """

  use SymbrellaWeb, :html

  # ---------- components ----------

  # Assistant bubble: show main text at normal size, and any lexical tail
  # ("By the way, …") in smaller, softer text.
  attr :message, :map, required: true
  def assistant_text(assigns) do
    ~H"""
    <% text = @message.text || "" %>
    <% {main, extra} = split_lexical_tail(text) %>

    <p class="whitespace-pre-wrap">
      <%= main %>
    </p>

    <%= if extra do %>
      <p class="mt-3 whitespace-pre-wrap text-xs sm:text-[13px] leading-relaxed opacity-80">
        <%= extra %>
      </p>
    <% end %>
    """
  end

  # Public entry point used by HomeLive.render/1
  def chat(assigns) do
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
                  <%= if m.role == :assistant do %>
                    <.assistant_text message={m} />
                  <% else %>
                    <p class="whitespace-pre-wrap"><%= m.text %></p>
                  <% end %>
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
          <!-- NOTE: removed phx-change from the form to avoid early patch races -->
          <form phx-submit="send" class="flex items-end gap-2">
            <textarea
              id="chat-input"
              name="message"
              phx-hook="ChatInput"
              phx-input="update_draft"
              phx-debounce="200"
              rows="1"
              placeholder="Type a message… (Enter to send, Shift+Enter for newline)"
              class="flex-1 resize-none rounded-2xl border border-slate-800/60 bg-[var(--color-panel)] px-4 py-3 text-[15px] outline-none focus:ring-2 focus:ring-[var(--color-accent)]/40"
              disabled={@bot_typing}
              aria-busy={@bot_typing}
              autocomplete="off"
              autocapitalize="off"
              autocorrect="off"
              spellcheck="false"
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

  # ---------- helpers ----------

  # Split an assistant reply into:
  #   {main_text, lexical_tail}
  # where lexical_tail starts at "By the way, …" if present.
  defp split_lexical_tail(text) when is_binary(text) do
    case String.split(text, "\n\nBy the way,", parts: 2) do
      [main, rest] ->
        {main, "By the way," <> rest}

      _ ->
        {text, nil}
    end
  end

  defp split_lexical_tail(other), do: {to_string(other), nil}
end

