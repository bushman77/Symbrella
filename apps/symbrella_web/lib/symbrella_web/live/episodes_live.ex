defmodule SymbrellaWeb.EpisodesLive do
  use SymbrellaWeb, :live_view

  alias SymbrellaWeb.EpisodesData

  @impl true
  def mount(_params, _session, socket) do
    episodes = load_episodes()

    {:ok,
     socket
     |> assign(:page_title, "Episodes")
     |> assign(:episodes_count, length(episodes))
     |> assign(:load_error, nil)
     |> stream(:episodes, episodes)}
  rescue
    e ->
      {:ok,
       socket
       |> assign(:page_title, "Episodes")
       |> assign(:episodes_count, 0)
       |> assign(:load_error, Exception.message(e))
       |> stream(:episodes, [])}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    episodes = load_episodes()

    {:noreply,
     socket
     |> assign(:episodes_count, length(episodes))
     |> assign(:load_error, nil)
     |> stream(:episodes, episodes, reset: true)}
  rescue
    e ->
      {:noreply,
       socket
       |> assign(:episodes_count, 0)
       |> assign(:load_error, Exception.message(e))
       |> stream(:episodes, [], reset: true)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={assigns[:current_scope]}>
      <section class="min-h-screen bg-zinc-950 px-4 py-6 text-zinc-100 sm:px-6 lg:px-8">
        <div class="mx-auto flex max-w-7xl flex-col gap-5">
          <header class="flex flex-col gap-4 border-b border-white/10 pb-5 md:flex-row md:items-end md:justify-between">
            <div>
              <p class="text-xs font-semibold uppercase tracking-[0.18em] text-cyan-300">
                Hippocampus
              </p>
              <h1 class="mt-2 text-3xl font-semibold tracking-normal text-white">Episodes</h1>
              <p class="mt-2 max-w-2xl text-sm leading-6 text-zinc-400">
                Persisted hippocampal episodes from the database, newest first.
              </p>
            </div>

            <div class="flex items-center gap-3">
              <span
                id="episodes-count"
                class="rounded-md border border-white/10 bg-white/[0.04] px-3 py-2 text-sm text-zinc-300"
              >
                {@episodes_count} rows
              </span>
              <button
                id="episodes-refresh"
                type="button"
                phx-click="refresh"
                class="inline-flex items-center gap-2 rounded-md bg-cyan-400 px-3 py-2 text-sm font-semibold text-zinc-950 transition hover:bg-cyan-300 focus:outline-none focus:ring-2 focus:ring-cyan-300 focus:ring-offset-2 focus:ring-offset-zinc-950"
              >
                <.icon name="hero-arrow-path" class="size-4" /> Refresh
              </button>
            </div>
          </header>

          <div
            :if={@load_error}
            id="episodes-error"
            class="rounded-md border border-red-400/30 bg-red-500/10 px-4 py-3 text-sm text-red-100"
          >
            Failed to load episodes: {@load_error}
          </div>

          <div class="overflow-hidden rounded-lg border border-white/10 bg-zinc-900/80 shadow-2xl shadow-black/30">
            <div class="grid grid-cols-[minmax(0,1fr)] border-b border-white/10 bg-zinc-900 px-4 py-3 text-xs font-semibold uppercase tracking-[0.16em] text-zinc-500 md:grid-cols-[180px_minmax(0,1.4fr)_minmax(0,1fr)_120px]">
              <div>Inserted</div>
              <div>Sentence / SI</div>
              <div>Tags</div>
              <div class="hidden md:block">Tokens</div>
            </div>

            <div id="episodes-list" phx-update="stream" class="divide-y divide-white/10">
              <div
                id="episodes-empty"
                class="hidden px-4 py-10 text-center text-sm text-zinc-500 only:block"
              >
                No episodes found.
              </div>

              <article
                :for={{id, episode} <- @streams.episodes}
                id={id}
                class="grid gap-4 px-4 py-4 transition hover:bg-white/[0.03] md:grid-cols-[180px_minmax(0,1.4fr)_minmax(0,1fr)_120px]"
              >
                <% copy_target_id = "episode-copy-#{episode.id}" %>
                <div class="text-sm text-zinc-400">
                  <time datetime={datetime_attr(episode.inserted_at)}>
                    {format_datetime(episode.inserted_at)}
                  </time>
                  <div class="mt-1 break-all font-mono text-[11px] text-zinc-600">
                    {episode.id}
                  </div>
                </div>

                <div class="min-w-0">
                  <div class="flex items-start justify-between gap-3">
                    <div class="break-words text-sm font-medium text-zinc-100">
                      {episode_sentence(episode)}
                    </div>
                    <button
                      id={"episode-copy-button-#{episode.id}"}
                      type="button"
                      phx-hook="ClipboardCopy"
                      data-clipboard-target={"##{copy_target_id}"}
                      title="Copy episode JSON"
                      class="inline-flex shrink-0 items-center gap-1.5 rounded-md border border-white/10 bg-white/[0.04] px-2.5 py-1.5 text-xs font-medium text-zinc-300 transition hover:border-cyan-300/40 hover:bg-cyan-400/10 hover:text-cyan-100 focus:outline-none focus:ring-2 focus:ring-cyan-300 focus:ring-offset-2 focus:ring-offset-zinc-900"
                    >
                      <.icon name="hero-clipboard-document" class="size-3.5" /> Copy
                    </button>
                  </div>
                  <pre
                    id={"episode-si-#{episode.id}"}
                    phx-no-curly-interpolation
                    class="mt-3 max-h-48 overflow-auto rounded-md border border-white/10 bg-black/30 p-3 text-xs leading-5 text-zinc-300"
                  ><%= pretty_map(episode.si) %></pre>
                  <pre id={copy_target_id} class="sr-only"><%= copy_json(episode) %></pre>
                </div>

                <div class="flex flex-wrap content-start gap-2">
                  <span
                    :for={tag <- episode.tags || []}
                    class="rounded bg-cyan-400/10 px-2 py-1 text-xs text-cyan-200 ring-1 ring-cyan-300/20"
                  >
                    {tag}
                  </span>
                  <span :if={(episode.tags || []) == []} class="text-sm text-zinc-600">none</span>
                </div>

                <div class="text-sm text-zinc-400">
                  <div class="font-semibold text-zinc-200">{episode.token_count || 0}</div>
                  <div class="mt-1 line-clamp-4 text-xs leading-5 text-zinc-500">
                    {Enum.join(episode.tokens || [], ", ")}
                  </div>
                </div>
              </article>
            </div>
          </div>
        </div>
      </section>
    </Layouts.app>
    """
  end

  defp load_episodes, do: EpisodesData.list_all()

  defp episode_sentence(%{sentence: text}) when is_binary(text) and text != "", do: text

  defp episode_sentence(%{si: %{} = si}) do
    value = Map.get(si, "sentence") || Map.get(si, :sentence)

    case value do
      text when is_binary(text) and text != "" -> text
      _ -> "(no sentence)"
    end
  end

  defp episode_sentence(_), do: "(no sentence)"

  defp pretty_map(%{} = map) do
    case Jason.encode(map, pretty: true) do
      {:ok, json} -> json
      _ -> inspect(map, pretty: true, limit: 80)
    end
  end

  defp pretty_map(value), do: inspect(value, pretty: true, limit: 80)

  defp copy_json(episode) do
    payload = %{
      id: episode.id,
      inserted_at: datetime_attr(episode.inserted_at),
      updated_at: datetime_attr(episode.updated_at),
      user_id: episode.user_id,
      session_id: episode.session_id,
      conversation_id: episode.conversation_id,
      source: episode.source,
      role: episode.role,
      sentence: episode.sentence,
      normalized_text: episode.normalized_text,
      intent: episode.intent,
      confidence: episode.confidence,
      tokens: episode.tokens || [],
      token_count: episode.token_count || 0,
      winners: episode.winners || %{},
      affect: episode.affect || %{},
      uncertainty: episode.uncertainty,
      tags: episode.tags || [],
      meta: episode.meta || %{},
      si: episode.si || %{}
    }

    case Jason.encode(payload, pretty: true) do
      {:ok, json} -> json
      _ -> inspect(payload, pretty: true, limit: :infinity)
    end
  end

  defp format_datetime(nil), do: "unknown"

  defp format_datetime(%NaiveDateTime{} = dt) do
    Calendar.strftime(dt, "%Y-%m-%d %H:%M:%S")
  end

  defp format_datetime(value), do: to_string(value)

  defp datetime_attr(%NaiveDateTime{} = dt), do: NaiveDateTime.to_iso8601(dt)
  defp datetime_attr(_), do: nil
end
