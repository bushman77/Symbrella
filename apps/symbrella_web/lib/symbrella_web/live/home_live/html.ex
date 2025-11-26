defmodule SymbrellaWeb.ChatLive.HTML do
  @moduledoc """
  Presentation for the chat LiveView (HomeLive). Pure HEEx & helpers only.

  Modal UX is implemented in `SymbrellaWeb.HomeLive.HTML.Modal`.
  This module delegates `explain_payload_for/1` for compatibility with callers.
  """

  use SymbrellaWeb, :html

  alias SymbrellaWeb.HomeLive.HTML.Modal

  # ---------- components ----------

  # Assistant bubble: show ONLY main text (lexical tail is modal-only).
  attr :message, :map, required: true
  def assistant_text(assigns) do
    ~H"""
    <% text = to_string(@message.text || "") %>
    <% {main, _extra} = split_lexical_tail(text) %>

    <p class="whitespace-pre-wrap">
      <%= main %>
    </p>
    """
  end

  # Explain button (passes id + text as fallback)
  attr :msg_id, :string, required: true
  attr :text, :string, default: ""
  def assistant_actions(assigns) do
    ~H"""
    <div class="mt-2 flex justify-end">
      <button
        type="button"
        phx-click="explain_open"
        phx-value-id={@msg_id}
        phx-value-text={@text}
        class="rounded-xl border border-slate-800/60 bg-[var(--color-panel)] px-3 py-1.5 text-xs font-semibold opacity-80 hover:opacity-100"
      >
        Explain
      </button>
    </div>
    """
  end

  # A single row in the message stream.
  attr :dom_id, :string, required: true
  attr :m, :map, required: true
  def message_row(assigns) do
    ~H"""
    <div id={@dom_id} class={row_class(@m)}>
      <div class={bubble_class(@m)}>
        <%= if @m.role == :assistant do %>
          <.assistant_text message={@m} />
          <.assistant_actions
            msg_id={message_id(@m, @dom_id)}
            text={to_string(@m.text || "")}
          />
        <% else %>
          <p class="whitespace-pre-wrap"><%= @m.text %></p>
        <% end %>
      </div>
    </div>
    """
  end

  # ---------- public entry point ----------

  attr :streams, :any, required: true
  attr :bot_typing, :boolean, default: false
  attr :draft, :string, default: ""
  attr :explain_open?, :boolean, default: false
  attr :explain_payload, :map, default: %{}
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
              <.message_row dom_id={dom_id} m={m} />
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

      <!-- EXPLAIN MODAL (single full-width panel) -->
      <Modal.explain_modal open?={@explain_open?} payload={@explain_payload} />
    </div>
    """
  end

  # ---------- compatibility ----------

  defdelegate explain_payload_for(message), to: Modal

  # ---------- helpers ----------

  defp row_class(m), do: if(m.role == :user, do: "flex justify-end", else: "flex justify-start")

  defp bubble_class(m) do
    if m.role == :user do
      "max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-sky-600/80 text-slate-50 shadow"
    else
      "max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-[var(--color-panel)] border border-slate-800/60 shadow"
    end
  end

  defp message_id(m, dom_id) do
    cond do
      is_binary(m[:id]) and m[:id] != "" -> m[:id]
      is_integer(m[:id]) -> Integer.to_string(m[:id])
      true -> dom_id
    end
  end

  # Keep this file lightweight: only used to hide the "By the way" tail in-bubble.
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
end

