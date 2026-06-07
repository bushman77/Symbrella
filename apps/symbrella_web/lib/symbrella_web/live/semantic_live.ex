defmodule SymbrellaWeb.SemanticLive do
  use SymbrellaWeb, :live_view

  alias Core.SemanticInput
  alias SymbrellaWeb.ChatHistory

  @default_sentence "Tell me about your working memory."
  @mode_options [{"Production pipeline", "prod"}, {"Test pipeline", "test"}]
  @si_fields %SemanticInput{} |> Map.from_struct() |> Map.keys() |> MapSet.new()

  @impl true
  def mount(_params, _session, socket) do
    {si, params, report_source} = latest_chat_report()

    {:ok,
     socket
     |> assign(:page_title, "Semantic Report")
      |> assign(:form, to_form(params, as: :semantic))
     |> assign(:report_source, report_source)
     |> assign_report(si, params)}
  end

  @impl true
  def handle_event("analyze", %{"semantic" => params}, socket) do
    normalized = normalize_params(params)
    si = resolve(normalized)

    {:noreply,
     socket
     |> assign(:form, to_form(normalized, as: :semantic))
     |> assign(:report_source, "Manual analysis")
     |> assign_report(si, normalized)}
  end

  @impl true
  def handle_event("load_latest_chat", _params, socket) do
    {si, params, report_source} = latest_chat_report()

    {:noreply,
     socket
     |> assign(:form, to_form(params, as: :semantic))
     |> assign(:report_source, report_source)
     |> assign_report(si, params)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={assigns[:current_scope]}>
      <section class="min-h-screen bg-stone-950 px-4 py-6 text-stone-100 sm:px-6 lg:px-8">
        <div class="mx-auto flex max-w-7xl flex-col gap-6">
          <header class="flex flex-col gap-4 border-b border-white/10 pb-5 lg:flex-row lg:items-end lg:justify-between">
            <div>
              <p class="text-xs font-semibold uppercase tracking-[0.18em] text-emerald-300">
                Core.SemanticInput
              </p>
              <h1 class="mt-2 text-3xl font-semibold tracking-normal text-white">
                Semantic Report
              </h1>
              <p class="mt-2 max-w-3xl text-sm leading-6 text-stone-400">
                A field-by-field report of the semantic carrier from the latest chat turn, with manual reruns available below.
              </p>
            </div>

            <div
              id="semantic-summary"
              class="grid grid-cols-2 gap-2 text-sm sm:grid-cols-4 lg:min-w-[520px]"
            >
              <.metric label="Fields" value={@field_count} />
              <.metric label="Tokens" value={@token_count} />
              <.metric label="Candidates" value={@candidate_count} />
              <.metric label="Trace" value={@trace_count} />
            </div>
          </header>

          <.form
            for={@form}
            id="semantic-report-form"
            phx-submit="analyze"
            class="grid gap-3 rounded-lg border border-white/10 bg-stone-900/75 p-4 shadow-2xl shadow-black/25 lg:grid-cols-[minmax(0,1fr)_220px_auto]"
          >
            <input type="hidden" name={@form[:session_id].name} value={@form[:session_id].value} />
            <.input
              field={@form[:sentence]}
              type="textarea"
              label="Input"
              rows="3"
              class="min-h-24 w-full resize-y rounded-md border border-white/10 bg-black/25 px-3 py-2 text-sm leading-6 text-stone-100 outline-none transition placeholder:text-stone-600 focus:border-emerald-300/60 focus:ring-2 focus:ring-emerald-300/20"
            />
            <.input
              field={@form[:mode]}
              type="select"
              label="Pipeline"
              options={@mode_options}
              class="w-full rounded-md border border-white/10 bg-black/25 px-3 py-2 text-sm text-stone-100 outline-none transition focus:border-emerald-300/60 focus:ring-2 focus:ring-emerald-300/20"
            />
            <div class="flex items-end">
              <div class="flex w-full flex-col gap-2 sm:flex-row lg:w-auto">
                <button
                  id="semantic-report-submit"
                  type="submit"
                  class="inline-flex w-full items-center justify-center gap-2 rounded-md bg-emerald-300 px-4 py-2.5 text-sm font-semibold text-stone-950 transition hover:bg-emerald-200 focus:outline-none focus:ring-2 focus:ring-emerald-200 focus:ring-offset-2 focus:ring-offset-stone-900 lg:w-auto"
                >
                  <.icon name="hero-document-magnifying-glass" class="size-4" /> Analyze
                </button>
                <button
                  id="semantic-load-latest-chat"
                  type="button"
                  phx-click="load_latest_chat"
                  class="inline-flex w-full items-center justify-center gap-2 rounded-md border border-white/10 bg-white/[0.04] px-4 py-2.5 text-sm font-semibold text-stone-200 transition hover:border-emerald-300/40 hover:bg-emerald-300/10 hover:text-emerald-100 focus:outline-none focus:ring-2 focus:ring-emerald-200 focus:ring-offset-2 focus:ring-offset-stone-900 lg:w-auto"
                >
                  <.icon name="hero-arrow-path" class="size-4" /> Latest
                </button>
              </div>
            </div>
          </.form>

          <section class="grid gap-4 lg:grid-cols-[minmax(0,0.95fr)_minmax(0,1.4fr)]">
            <div class="rounded-lg border border-white/10 bg-stone-900/70 p-4">
              <h2 class="text-sm font-semibold uppercase tracking-[0.16em] text-stone-500">
                Overview
              </h2>
              <dl id="semantic-overview" class="mt-4 grid gap-3 text-sm">
                <.fact label="Report" value={@report_source} />
                <.fact label="Sentence" value={@si.sentence || ""} />
                <.fact label="Source" value={format_value(@si.source)} />
                <.fact label="Intent" value={format_value(@si.intent)} />
                <.fact label="Keyword" value={format_value(@si.keyword)} />
                <.fact label="Confidence" value={format_value(@si.confidence)} />
                <.fact label="Session" value={format_value(@si.session_id)} />
              </dl>
            </div>

            <div class="rounded-lg border border-white/10 bg-stone-900/70 p-4">
              <div class="flex items-center justify-between gap-3">
                <h2 class="text-sm font-semibold uppercase tracking-[0.16em] text-stone-500">
                  Tokens
                </h2>
                <span class="rounded bg-white/[0.04] px-2 py-1 text-xs text-stone-400">
                  {@token_count} total
                </span>
              </div>
              <div id="semantic-tokens" class="mt-4 flex flex-wrap gap-2">
                <span
                  :if={@si.tokens == []}
                  id="semantic-tokens-empty"
                  class="text-sm text-stone-500"
                >
                  No tokens returned.
                </span>
                <span
                  :for={token <- @token_reports}
                  id={"semantic-token-#{token.index}"}
                  class="rounded-md border border-emerald-300/20 bg-emerald-300/10 px-2.5 py-1.5 text-xs text-emerald-100"
                >
                  <span class="font-mono text-emerald-300">#{token.index}</span>
                  {token.label}
                </span>
              </div>
            </div>
          </section>

          <section class="rounded-lg border border-white/10 bg-stone-900/70">
            <div class="border-b border-white/10 px-4 py-3">
              <h2 class="text-sm font-semibold uppercase tracking-[0.16em] text-stone-500">
                Complete Field Report
              </h2>
            </div>

            <div id="semantic-field-report" class="divide-y divide-white/10">
              <article
                :for={field <- @field_reports}
                id={"semantic-field-#{field.name}"}
                class="grid gap-3 px-4 py-4 transition hover:bg-white/[0.025] lg:grid-cols-[220px_minmax(0,1fr)]"
              >
                <div>
                  <div class="font-mono text-sm font-semibold text-emerald-200">
                    {field.name}
                  </div>
                  <div class="mt-1 text-xs uppercase tracking-[0.14em] text-stone-600">
                    {field.kind}
                  </div>
                </div>
                <pre class="max-h-80 overflow-auto rounded-md border border-white/10 bg-black/30 p-3 text-xs leading-5 text-stone-300"><%= field.value %></pre>
              </article>
            </div>
          </section>
        </div>
      </section>
    </Layouts.app>
    """
  end

  defp assign_report(socket, %SemanticInput{} = si, params) do
    field_reports = field_reports(si)

    socket
    |> assign(:si, si)
    |> assign(:params, params)
    |> assign(:mode_options, @mode_options)
    |> assign(:field_reports, field_reports)
    |> assign(:field_count, length(field_reports))
    |> assign(:token_reports, token_reports(si.tokens))
    |> assign(:token_count, length(si.tokens))
    |> assign(:candidate_count, candidate_count(si.sense_candidates))
    |> assign(:trace_count, length(si.trace || []))
  end

  defp resolve(params) do
    normalized = normalize_params(params)

    normalized["sentence"]
    |> Core.resolve_input(resolve_opts(normalized))
    |> maybe_put_session_id(normalized["session_id"])
  end

  defp normalize_params(params) do
    sentence =
      params
      |> Map.get("sentence", @default_sentence)
      |> case do
        text when is_binary(text) -> String.trim(text)
        _ -> @default_sentence
      end
      |> case do
        "" -> @default_sentence
        text -> text
      end

    %{
      "sentence" => sentence,
      "mode" => normalize_mode(Map.get(params, "mode", "prod")),
      "session_id" => normalize_session_id(Map.get(params, "session_id"))
    }
  end

  defp normalize_mode("prod"), do: "prod"
  defp normalize_mode(_), do: "test"

  defp normalize_session_id(session_id) when is_binary(session_id), do: session_id
  defp normalize_session_id(_), do: nil

  defp resolve_opts(%{"mode" => "prod"} = params) do
    [
      mode: :prod,
      enrich_lexicon?: true,
      lexicon_stage?: true,
      session_id: params["session_id"]
    ]
  end

  defp resolve_opts(_params), do: [mode: :test]

  defp maybe_put_session_id(%SemanticInput{} = si, nil), do: si
  defp maybe_put_session_id(%SemanticInput{} = si, session_id), do: %{si | session_id: session_id}

  defp latest_chat_report do
    messages = ChatHistory.list()

    case latest_semantic_snapshot(messages) do
      {%SemanticInput{} = si, message} ->
        params = params_from_si(si, "prod")
        {si, params, "Latest chat semantic snapshot #{message.id}"}

      nil ->
        case latest_user_message(messages) do
          %{text: text} = message ->
            params = %{
              "sentence" => text,
              "mode" => "prod",
              "session_id" => normalize_session_id(Map.get(message, :session_id))
            }

            {resolve(params), params, "Latest chat user rerun #{message.id}"}

          nil ->
            params = %{"sentence" => @default_sentence, "mode" => "test", "session_id" => nil}
            {resolve(params), params, "Sample input"}
        end
    end
  end

  defp latest_semantic_snapshot(messages) do
    messages
    |> Enum.reverse()
    |> Enum.find_value(fn
      %{si: si} = message -> {coerce_si(si), message}
      _message -> nil
    end)
  end

  defp latest_user_message(messages) do
    messages
    |> Enum.reverse()
    |> Enum.find(fn
      %{role: :user, text: text} when is_binary(text) and text != "" -> true
      _message -> false
    end)
  end

  defp coerce_si(%SemanticInput{} = si), do: si

  defp coerce_si(%{} = map) do
    filtered =
      map
      |> Enum.filter(fn {key, _value} -> is_atom(key) and MapSet.member?(@si_fields, key) end)
      |> Map.new()

    struct(SemanticInput, filtered)
  end

  defp params_from_si(%SemanticInput{} = si, mode) do
    %{
      "sentence" => si.sentence || @default_sentence,
      "mode" => mode,
      "session_id" => normalize_session_id(si.session_id)
    }
  end

  defp field_reports(%SemanticInput{} = si) do
    si
    |> Map.from_struct()
    |> Enum.sort_by(fn {key, _value} -> Atom.to_string(key) end)
    |> Enum.map(fn {key, value} ->
      %{
        name: Atom.to_string(key),
        kind: value_kind(value),
        value: format_value(value)
      }
    end)
  end

  defp token_reports(tokens) when is_list(tokens) do
    tokens
    |> Enum.with_index()
    |> Enum.map(fn {token, index} ->
      %{index: index, label: token_label(token)}
    end)
  end

  defp candidate_count(candidates) when is_map(candidates) do
    candidates
    |> Map.values()
    |> Enum.reduce(0, fn
      list, count when is_list(list) -> count + length(list)
      _other, count -> count
    end)
  end

  defp candidate_count(_), do: 0

  defp token_label(%{phrase: phrase}) when is_binary(phrase), do: phrase
  defp token_label(%{norm: norm}) when is_binary(norm), do: norm
  defp token_label(%{"phrase" => phrase}) when is_binary(phrase), do: phrase
  defp token_label(%{"norm" => norm}) when is_binary(norm), do: norm
  defp token_label(token) when is_binary(token), do: token
  defp token_label(token), do: inspect(token, limit: 8)

  defp value_kind(nil), do: "nil"
  defp value_kind(value) when is_binary(value), do: "string"
  defp value_kind(value) when is_atom(value), do: "atom"
  defp value_kind(value) when is_number(value), do: "number"
  defp value_kind(value) when is_boolean(value), do: "boolean"
  defp value_kind(value) when is_list(value), do: "list / #{length(value)}"
  defp value_kind(value) when is_map(value), do: "map / #{map_size(value)}"
  defp value_kind(_value), do: "term"

  defp format_value(nil), do: "nil"

  defp format_value(value) do
    inspect(value, pretty: true, limit: :infinity, printable_limit: :infinity)
  end

  attr :label, :string, required: true
  attr :value, :any, required: true

  defp metric(assigns) do
    ~H"""
    <div class="rounded-md border border-white/10 bg-white/[0.04] px-3 py-2">
      <div class="text-xs uppercase tracking-[0.14em] text-stone-500">{@label}</div>
      <div class="mt-1 font-mono text-lg font-semibold text-white">{@value}</div>
    </div>
    """
  end

  attr :label, :string, required: true
  attr :value, :string, required: true

  defp fact(assigns) do
    ~H"""
    <div class="grid gap-1 border-b border-white/10 pb-3 last:border-b-0 last:pb-0 sm:grid-cols-[120px_minmax(0,1fr)]">
      <dt class="text-xs uppercase tracking-[0.14em] text-stone-600">{@label}</dt>
      <dd class="min-w-0 break-words font-mono text-sm text-stone-200">{@value}</dd>
    </div>
    """
  end
end
