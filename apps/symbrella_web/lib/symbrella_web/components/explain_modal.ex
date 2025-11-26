# apps/symbrella_web/lib/symbrella_web/components/explain_modal.ex
defmodule SymbrellaWeb.Components.ExplainModal do
  use SymbrellaWeb, :html

  @doc """
  payload shape (flexible):
    %{
      title: "Explanation",
      sections: [
        %{key: :relationships, title: "Synonyms & Antonyms", items: [%{label: "...", body: "..."}]},
        %{key: :tone,          title: "Tone decision",       items: [%{label: "...", body: "..."}]},
        ...
      ]
    }
  """

  attr :open?, :boolean, default: false
  attr :payload, :map, default: %{}
  attr :on_close, :any, default: "explain_close" # LiveView event name

  def explain_modal(assigns) do
    assigns =
      assigns
      |> assign_new(:payload, fn -> %{} end)
      |> assign_new(:open?, fn -> false end)

    ~H"""
    <div :if={@open?} class="fixed inset-0 z-[80]">
      <!-- backdrop -->
      <div
        class="absolute inset-0 bg-black/60"
        phx-click={@on_close}
        aria-hidden="true"
      />

      <!-- dialog -->
      <div
        class="absolute inset-0 flex items-end sm:items-center justify-center p-3 sm:p-6"
        role="dialog"
        aria-modal="true"
        phx-window-keydown={@on_close}
        phx-key="escape"
      >
        <div class="w-full sm:max-w-2xl rounded-2xl border border-zinc-800 bg-zinc-950 shadow-xl">
          <!-- header -->
          <div class="flex items-center justify-between gap-3 px-4 py-3 border-b border-zinc-800">
            <div class="min-w-0">
              <div class="text-sm font-semibold text-zinc-100 truncate">
                <%= @payload[:title] || "Explain" %>
              </div>
              <div :if={@payload[:subtitle]} class="text-xs text-zinc-400 truncate">
                <%= @payload[:subtitle] %>
              </div>
            </div>

            <button
              type="button"
              class="shrink-0 rounded-xl border border-zinc-800 bg-zinc-900 px-3 py-1.5 text-xs font-semibold text-zinc-200 hover:bg-zinc-800"
              phx-click={@on_close}
              aria-label="Close"
            >
              Close
            </button>
          </div>

          <!-- scrollable body -->
          <div class="max-h-[75vh] overflow-y-auto px-4 py-3">
            <div class="grid gap-2">
              <div
                :for={sec <- (@payload[:sections] || [])}
                class="rounded-2xl border border-zinc-800 bg-zinc-950/60 p-3"
              >
                <div class="flex items-center justify-between gap-3">
                  <div class="text-sm font-semibold text-zinc-100">
                    <%= sec[:title] || to_string(sec[:key] || "Section") %>
                  </div>
                  <span :if={sec[:tag]} class="rounded-full border border-zinc-800 bg-zinc-900 px-2 py-0.5 text-[11px] text-zinc-300">
                    <%= sec[:tag] %>
                  </span>
                </div>

                <div :if={sec[:hint]} class="mt-1 text-xs text-zinc-400">
                  <%= sec[:hint] %>
                </div>

                <div class="mt-2 grid gap-2">
                  <div
                    :for={item <- (sec[:items] || [])}
                    class="rounded-xl border border-zinc-800 bg-zinc-950 p-3"
                  >
                    <div :if={item[:label]} class="text-xs font-semibold text-zinc-200">
                      <%= item[:label] %>
                    </div>
                    <div class="text-sm text-zinc-300 whitespace-pre-wrap break-words">
                      <%= item[:body] || "" %>
                    </div>
                  </div>

                  <div :if={(sec[:items] || []) == []} class="text-sm text-zinc-500">
                    No details for this section.
                  </div>
                </div>
              </div>

              <div :if={(@payload[:sections] || []) == []} class="text-sm text-zinc-500">
                No explanation available for this message.
              </div>
            </div>
          </div>

          <!-- footer -->
          <div class="px-4 py-3 border-t border-zinc-800 flex justify-end">
            <button
              type="button"
              class="rounded-xl border border-zinc-800 bg-zinc-900 px-3 py-1.5 text-xs font-semibold text-zinc-200 hover:bg-zinc-800"
              phx-click={@on_close}
            >
              Close
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end
end

