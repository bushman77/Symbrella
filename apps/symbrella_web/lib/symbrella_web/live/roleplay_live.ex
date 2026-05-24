defmodule SymbrellaWeb.RoleplayLive do
  use SymbrellaWeb, :live_view

  @default_system_prompt """
  You are Symbrella's roleplay engine. You run fictional scenes.

  You are not a helper chatbot.
  You are not a customer-support assistant.
  You are not the character named Symbrella.
  Do not introduce yourself.
  Do not say "How may I assist you today?"
  Do not say "Is there anything else I can help you with?"

  Core role:
  - You control the scene, setting, environment, NPCs, creatures, and consequences.
  - The user controls only their own character.
  - The user's message is an attempted action, spoken line, or claim. It is not automatically successful.
  - You decide outcomes based on scene logic, NPC resistance, physical limits, social consequences, danger, and plausibility.

  Player identity boundary:
  - Do not assign the user character a gender, age, name, body type, voice, clothing, backstory, emotional state, or social role unless the user provides it.
  - Do not call the user "sir", "ma'am", "little lady", "boy", "girl", "man", "woman", or similar identity labels unless established by the user.
  - Do not invent the user's movement through the scene.
  - Do not write internal body reactions such as racing heart, shuddering, breathing, nausea, arousal, fear, relief, or embarrassment.
  - If the user's message is only a greeting, meta-comment, or too vague to establish a scene, ask one concise setup question instead of starting a scene.

  Player agency:
  - Never write the user's dialogue.
  - Never write the user's thoughts.
  - Never write the user's feelings.
  - Never write the user's intentions.
  - Never decide that the user feels embarrassed, afraid, ashamed, satisfied, amused, regretful, or excited.
  - Never write lines like "you said", "you explained", "you asked", "you felt", "you realized", or "you decided" unless directly restating an action the user already wrote.

  Action adjudication:
  - Do not treat extreme actions as automatically completed.
  - If the user attempts violence, chaos, humiliation, destruction, or impossible behavior, narrate the attempt and the world's immediate reaction.
  - NPCs may dodge, resist, flee, interrupt, counterattack, call for help, trigger alarms, use magic wards, or otherwise prevent success.
  - Do not reward escalating chaos with easy success.
  - Do not summarize what the user should do next.

  Violence and gross-out handling:
  - Keep violence non-graphic.
  - Do not describe gore, dismemberment, mutilation, exposed wounds, or body desecration in detail.
  - For extreme gore or desecration attempts, cut away, interrupt, or narrate non-graphic consequences.
  - Gross-out comedy may exist, but keep it brief and in-world.
  - Do not become a moralizing assistant. Let the world react.

  Scene behavior:
  - Stay inside the fictional scene.
  - Let NPCs disagree, refuse, bargain, threaten, flee, laugh, or react according to their character.
  - End each reply with the scene still open for the user to act.

  Formatting:
  - Do not write future user turns.
  - Do not include chat-template tokens.
  - Keep replies under 180 words.
  """

  @impl true
  def mount(_params, _session, socket) do
    initial_message = %{
      id: "rp-welcome",
      role: :assistant,
      text: "Roleplay gateway online. MythosMax is ready through Symbrella."
    }

    {:ok,
     socket
     |> assign(
       page_title: "Roleplay",
       draft: "",
       bot_typing: false,
       pending_task: nil,
       cancelled_ref: nil,
       system_prompt: String.trim(@default_system_prompt),
       messages_for_llm: []
     )
     |> stream(:messages, [initial_message])}
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
        user_id = unique_id("rp-u")

        user_message = %{
          id: user_id,
          role: :user,
          text: text
        }

        messages_for_llm =
          socket.assigns.messages_for_llm ++
            [%{"role" => "user", "content" => text}]

        system_prompt = socket.assigns.system_prompt

        socket =
          socket
          |> stream_insert(:messages, user_message)
          |> assign(
            draft: "",
            bot_typing: true,
            cancelled_ref: nil,
            messages_for_llm: messages_for_llm
          )
          |> push_event("chat:scroll", %{to: "composer"})

        task =
          Task.Supervisor.async_nolink(Symbrella.TaskSup, fn ->
            run_roleplay_turn(system_prompt, messages_for_llm)
          end)

        {:noreply, assign(socket, :pending_task, task)}
    end
  end

  @impl true
  def handle_event("stop", _params, socket) do
    case socket.assigns.pending_task do
      %Task{} = task ->
        _ = Task.shutdown(task, :brutal_kill)
        Process.demonitor(task.ref, [:flush])

        stopped = %{
          id: unique_id("rp-x"),
          role: :assistant,
          text: "(stopped)"
        }

        {:noreply,
         socket
         |> assign(
           bot_typing: false,
           cancelled_ref: task.ref,
           pending_task: nil
         )
         |> stream_insert(:messages, stopped)
         |> push_event("chat:scroll", %{to: "composer"})}

      _ ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_info(
        {ref, {:ok, assistant_text}},
        %{assigns: %{pending_task: %Task{ref: ref}}} = socket
      ) do
    Process.demonitor(ref, [:flush])

    assistant_id = unique_id("rp-b")

    assistant_message = %{
      id: assistant_id,
      role: :assistant,
      text: String.trim(to_string(assistant_text))
    }

    messages_for_llm =
      socket.assigns.messages_for_llm ++
        [%{"role" => "assistant", "content" => assistant_message.text}]

    {:noreply,
     socket
     |> assign(
       bot_typing: false,
       pending_task: nil,
       messages_for_llm: messages_for_llm
     )
     |> stream_insert(:messages, assistant_message)
     |> push_event("chat:scroll", %{to: "composer"})}
  end

  @impl true
  def handle_info({ref, {:error, reason}}, %{assigns: %{pending_task: %Task{ref: ref}}} = socket) do
    Process.demonitor(ref, [:flush])

    error_message = %{
      id: unique_id("rp-e"),
      role: :assistant,
      text: "Roleplay gateway error: #{inspect(reason)}"
    }

    {:noreply,
     socket
     |> assign(bot_typing: false, pending_task: nil)
     |> stream_insert(:messages, error_message)
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
        error_message = %{
          id: unique_id("rp-down"),
          role: :assistant,
          text: "Roleplay task crashed: #{Exception.format_exit(reason)}"
        }

        {:noreply,
         socket
         |> assign(bot_typing: false, pending_task: nil)
         |> stream_insert(:messages, error_message)
         |> push_event("chat:scroll", %{to: "composer"})}

      true ->
        {:noreply, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id="roleplay-root"
      class="relative h-[100svh] min-h-[100svh] bg-[var(--color-bg)] text-[var(--color-text)]"
    >
      <header
        id="chat-header"
        phx-hook="HeaderSizer"
        class="fixed top-0 left-0 right-0 z-20 border-b border-slate-800/60 bg-[var(--color-bg)]/90 backdrop-blur"
      >
        <div class="mx-auto max-w-4xl w-full px-4 py-3 flex items-center justify-between">
          <div>
            <h1 class="text-base sm:text-lg font-semibold">Symbrella · Roleplay</h1>
            <p class="text-xs opacity-70">MythosMax through llama.cpp, mediated by Symbrella</p>
          </div>

          <a href="/" class="text-xs opacity-70 hover:opacity-100">
            Chat
          </a>
        </div>
      </header>

      <main
        id="messages"
        phx-hook="ScrollOnEvent"
        class="absolute left-0 right-0 overflow-y-auto scroll-smooth"
        style="top: var(--hdr,64px); bottom: var(--ftr,72px);"
      >
        <div class="mx-auto max-w-4xl w-full px-3 sm:px-4 py-4">
          <div id="message-list" phx-update="stream" class="space-y-3">
            <%= for {dom_id, m} <- @streams.messages do %>
              <div id={dom_id} class={row_class(m)}>
                <div class={bubble_class(m)}>
                  <p class="whitespace-pre-wrap">{m.text}</p>
                </div>
              </div>
            <% end %>
          </div>

          <%= if @bot_typing do %>
            <div id="typing" class="mt-3 flex justify-start">
              <div class="max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-[var(--color-panel)] border border-slate-800/60 shadow">
                <span class="opacity-70">MythosMax is speaking through Symbrella…</span>
              </div>
            </div>
          <% end %>

          <div id="bottom"></div>
        </div>
      </main>

      <footer
        id="chat-composer"
        phx-hook="FooterSizer"
        class="fixed bottom-0 left-0 right-0 z-30 border-t border-slate-800/60 bg-[var(--color-bg)]/95 backdrop-blur"
      >
        <div class="mx-auto max-w-4xl w-full px-3 sm:px-4 pt-2 pb-3">
          <form phx-submit="send" class="flex items-end gap-2">
            <textarea
              id="roleplay-input"
              name="message"
              phx-hook="ChatInput"
              phx-input="update_draft"
              phx-debounce="200"
              rows="1"
              placeholder="Enter the scene…"
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
              <button type="submit" class="btn px-4 py-3 rounded-2xl shadow">
                Send
              </button>
            <% end %>
          </form>
        </div>
      </footer>
    </div>
    """
  end

  defp run_roleplay_turn(system_prompt, messages_for_llm) do
    request = %{
      "model" => "symbrella-rp",
      "stream" => false,
      "temperature" => 0.8,
      "messages" => [
        %{"role" => "system", "content" => system_prompt}
        | messages_for_llm
      ]
    }

    case Core.RoleplayTurn.run(request) do
      {:ok, %{choices: [%{message: %{content: content}} | _]}} ->
        {:ok, content}

      {:ok, other} ->
        {:error, {:unexpected_roleplay_response, other}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp sanitize_user_text(text) do
    text
    |> to_string()
    |> String.replace(~r/\r\n?/, "\n")
    |> String.replace(~r/[ \t]+(\n)/, "\\1")
    |> String.replace(~r/\n{3,}/, "\n\n")
    |> String.trim()
  end

  defp unique_id(prefix) do
    prefix <> "-" <> Integer.to_string(System.unique_integer([:positive]))
  end

  defp row_class(%{role: :user}), do: "flex justify-end"
  defp row_class(_), do: "flex justify-start"

  defp bubble_class(%{role: :user}) do
    "max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-sky-600/80 text-slate-50 shadow"
  end

  defp bubble_class(_) do
    "max-w-[85%] sm:max-w-[70%] rounded-2xl px-4 py-2 bg-[var(--color-panel)] border border-slate-800/60 shadow"
  end
end
