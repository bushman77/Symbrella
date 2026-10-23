defmodule SymbrellaWeb.SocialLive do
  @moduledoc """
  Protected read-only Colony topic browser. Selecting a topic retrieves only
  its public discussion. No drafting, posting, or cognitive persistence.
  """

  use SymbrellaWeb, :live_view

  alias SymbrellaWeb.{SocialData, SocialCognitiveReport}

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       page_title: "Social · Colony",
       page: 1,
       post_id: nil,
       topics: [],
       topic: nil,
       comments: [],
       comments_truncated?: false,
       has_next?: false,
       loading?: true,
       error: nil,
       analysis_open?: false,
       cognitive_report: nil
     )}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    page = parse_page(Map.get(params, "page"))
    socket = assign(socket, page: page, error: nil, loading?: true, analysis_open?: false, cognitive_report: nil)

    socket =
      case socket.assigns.live_action do
        :show ->
          socket = assign(socket, post_id: Map.get(params, "post_id"), topic: nil, comments: [])
          if connected?(socket), do: fetch_thread(socket), else: socket

        _ ->
          socket = assign(socket, post_id: nil, topics: [])
          if connected?(socket), do: fetch_topics(socket), else: socket
      end

    {:noreply, socket}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    socket =
      if socket.assigns.live_action == :show,
        do: fetch_thread(socket),
        else: fetch_topics(socket)

    {:noreply, socket}
  end

  @impl true
  def handle_event("analyze_cognition", _params, socket) do
    if socket.assigns.live_action == :show and socket.assigns.topic do
      report_input = %{topic: socket.assigns.topic, comments: socket.assigns.comments}
      analyzer = Application.get_env(:symbrella_web, :social_cognitive_analyzer, SocialCognitiveReport)

      case analyzer.analyze(report_input) do
        {:ok, report} ->
          {:noreply, assign(socket, analysis_open?: true, cognitive_report: report)}

        _ ->
          {:noreply, assign(socket, analysis_open?: true, cognitive_report: nil,
            error: "Read-only cognitive analysis is unavailable.")}
      end
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("close_analysis", _params, socket) do
    {:noreply, assign(socket, analysis_open?: false)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <Layouts.app flash={@flash} current_scope={assigns[:current_scope]}>
      <section id="social-page" class="min-h-screen bg-zinc-950 px-4 py-6 text-zinc-100 sm:px-6 lg:px-8">
        <div class="mx-auto flex max-w-6xl flex-col gap-5">
          <header class="flex flex-wrap items-end justify-between gap-4 border-b border-white/10 pb-5">
            <div class="min-w-0">
              <p class="text-xs font-semibold uppercase tracking-[0.18em] text-cyan-300">
                Colony · read only
              </p>
              <h1 class="mt-2 text-3xl font-semibold text-white">
                {if @live_action == :show, do: "Conversation", else: "Social"}
              </h1>
              <p class="mt-2 max-w-2xl text-sm text-zinc-400">
                Browse external discussions without publishing or altering Symbrella's memory.
              </p>
            </div>
            <div class="flex flex-wrap items-center gap-2 text-sm">
              <.link navigate={~p"/"} class="rounded-md border border-white/10 px-3 py-2 text-zinc-200 hover:bg-white/10">
                Chat
              </.link>
              <.link
                :if={@live_action == :show}
                navigate={~p"/social?page=#{@page}"}
                id="social-back"
                class="rounded-md border border-white/10 px-3 py-2 text-zinc-200 hover:bg-white/10"
              >
                Back to topics
              </.link>
              <button
                :if={@live_action == :show and @topic}
                id="social-analyze"
                type="button"
                phx-click="analyze_cognition"
                class="rounded-md border border-cyan-400/50 px-3 py-2 font-semibold text-cyan-200 hover:bg-cyan-400/10"
              >
                Analyze cognition
              </button>
              <button
                id="social-refresh"
                type="button"
                phx-click="refresh"
                class="rounded-md bg-cyan-400 px-3 py-2 font-semibold text-zinc-950 hover:bg-cyan-300"
              >
                Refresh
              </button>
            </div>
          </header>

          <div
            :if={@error}
            id="social-error"
            role="alert"
            class="rounded-lg border border-rose-400/30 bg-rose-400/10 px-4 py-3 text-sm text-rose-100"
          >
            {@error}
          </div>
          <p :if={@loading?} id="social-loading" class="py-8 text-center text-zinc-400">
            Loading Colony conversations…
          </p>

          <section :if={@live_action != :show} id="social-index" class="flex flex-col gap-4">
            <div class="flex flex-wrap items-center justify-between gap-3">
              <p id="social-page-count" class="text-sm text-zinc-400">
                Newest topics · Page {@page} · {length(@topics)} shown · up to {SocialData.page_size()} per page
              </p>
              <nav aria-label="Topic pages" class="flex gap-2 text-sm">
                <.link
                  :if={@page > 1}
                  id="social-previous"
                  patch={~p"/social?page=#{@page - 1}"}
                  class="rounded-md border border-white/10 px-3 py-2 hover:bg-white/10"
                >
                  Previous
                </.link>
                <.link
                  :if={@has_next?}
                  id="social-next"
                  patch={~p"/social?page=#{@page + 1}"}
                  class="rounded-md border border-white/10 px-3 py-2 hover:bg-white/10"
                >
                  Next 25
                </.link>
              </nav>
            </div>

            <div id="social-list" class="divide-y divide-white/10 overflow-hidden rounded-lg border border-white/10 bg-zinc-900/70">
              <p :if={!@loading? and @topics == [] and is_nil(@error)} id="social-empty" class="px-5 py-12 text-center text-zinc-400">
                No topics were returned for this page.
              </p>
              <article :for={topic <- @topics} id={"social-topic-#{topic.id}"} class="min-w-0 transition hover:bg-white/[0.04]">
                <.link
                  navigate={~p"/social/#{topic.id}?page=#{@page}"}
                  class="block min-w-0 px-4 py-4 sm:px-5"
                >
                  <div class="flex flex-wrap items-start justify-between gap-2">
                    <h2 class="min-w-0 break-words text-base font-semibold leading-6 text-zinc-100">
                      {topic.title}
                    </h2>
                    <span class="shrink-0 text-xs text-zinc-500">{topic.created_at}</span>
                  </div>
                  <div class="mt-1 flex flex-wrap gap-3 text-xs text-cyan-200">
                    <span>By {topic.author}</span>
                    <span :if={topic.colony not in [nil, ""]}>Colony: {topic.colony}</span>
                  </div>
                  <p :if={topic.body not in [nil, ""]} class="mt-2 line-clamp-2 whitespace-pre-wrap break-words text-sm leading-6 text-zinc-400">
                    {preview(topic.body)}
                  </p>
                </.link>
              </article>
            </div>
          </section>

          <section :if={@live_action == :show} id="social-thread" class="flex flex-col gap-5">
            <article :if={@topic} id="social-post" class="min-w-0 rounded-lg border border-white/10 bg-zinc-900/75 p-4 sm:p-6">
              <div class="flex flex-wrap items-center gap-x-3 gap-y-1 text-xs text-cyan-200">
                <span>By {@topic.author}</span>
                <span :if={@topic.colony != ""}>Colony: {@topic.colony}</span>
                <span class="text-zinc-500">{@topic.created_at}</span>
              </div>
              <h2 class="mt-3 break-words text-xl font-semibold text-white">{@topic.title}</h2>
              <p class="mt-4 whitespace-pre-wrap break-words text-sm leading-7 text-zinc-200">{@topic.body}</p>
            </article>

            <div :if={@topic} class="flex items-center justify-between gap-2">
              <h3 class="font-semibold text-zinc-100">Discussion</h3>
              <span id="social-replies-count" class="text-xs text-zinc-400">
                {length(@comments)} replies returned
              </span>
            </div>
            <p :if={@topic && @comments == []} class="rounded-lg border border-white/10 p-6 text-center text-sm text-zinc-500">
              No replies were returned for this topic.
            </p>
            <p :if={@comments_truncated?} class="text-sm text-amber-200">
              Display limited to the first 200 returned replies.
            </p>
            <div id="social-comments" class="flex flex-col gap-3">
              <article
                :for={{comment, index} <- Enum.with_index(@comments)}
                id={"social-comment-#{index}"}
                class="min-w-0 rounded-lg border border-white/10 bg-zinc-900/70 px-4 py-4"
              >
                <div class="flex flex-wrap items-center justify-between gap-2 text-xs">
                  <span class="font-semibold text-cyan-200">{comment.author}</span>
                  <span class="text-zinc-500">{comment.created_at}</span>
                </div>
                <p :if={comment.parent_id != ""} class="mt-1 text-xs text-zinc-500">
                  Reply to another comment
                </p>
                <p class="mt-3 whitespace-pre-wrap break-words text-sm leading-6 text-zinc-200">
                  {comment.body}
                </p>
              </article>
            </div>
          </section>
          <div
            :if={@analysis_open?}
            id="social-cognitive-modal"
            class="fixed inset-0 z-50 flex items-center justify-center bg-black/80 p-2 sm:p-5"
            role="dialog"
            aria-modal="true"
            aria-label="Colony cognitive analysis"
          >
            <div class="flex w-full max-w-6xl min-h-0 min-w-0 flex-col overflow-hidden rounded-xl border border-cyan-300/30 bg-zinc-950 text-zinc-100 shadow-2xl"
              style="height: calc(100vh - 2rem); height: calc(100dvh - 2rem); max-height: 900px;">
              <header class="flex shrink-0 items-center justify-between gap-3 border-b border-white/10 px-4 py-4 sm:px-6">
                <div class="min-w-0">
                  <h2 class="text-lg font-semibold">Cognitive inspection</h2>
                  <p class="break-words text-xs text-zinc-400">External Colony observations · isolated read-only semantic shadow · no learning or posting</p>
                </div>
                <div class="flex shrink-0 items-center gap-2">
                  <button id="social-copy-analysis" type="button" phx-hook="ClipboardCopy"
                    data-clipboard-target="#social-report-content"
                    class="rounded border border-cyan-400/50 px-3 py-2 text-cyan-200 hover:bg-cyan-400/10">Copy report</button>
                  <button id="social-close-analysis" type="button" phx-click="close_analysis" aria-label="Close analysis"
                    class="rounded border border-white/20 px-3 py-2 hover:bg-white/10">Close</button>
                </div>
              </header>
              <div id="social-report-content" tabindex="0" class="min-h-0 flex-1 space-y-4 overflow-y-auto overflow-x-hidden overscroll-contain px-4 py-5 pb-8 sm:px-6"
                style="display: block; flex: 1 1 0%; min-height: 0; overflow-y: scroll; overflow-x: hidden; scrollbar-width: auto;">
                <%= if @cognitive_report do %>
                  <p class="text-sm text-zinc-400">
                    {@cognitive_report.analyzed_segments} sentence samples inspected · {@cognitive_report.omitted_replies} replies omitted · {Map.get(@cognitive_report, :omitted_sentences, 0)} sentence samples omitted.
                    Lexical selections are hypotheses, not verified facts.
                  </p>
                  <section :if={Map.has_key?(@cognitive_report, :understanding)} id="social-understanding"
                    class="space-y-3 rounded-lg border border-cyan-400/25 bg-cyan-950/10 p-4 sm:p-5">
                    <p class="text-xs font-semibold uppercase tracking-widest text-cyan-300">Symbrella's understanding · evidence constrained</p>
                    <h3 class="break-words text-lg font-semibold">{@cognitive_report.understanding.headline}</h3>
                    <p class="text-sm leading-6 text-zinc-300">{@cognitive_report.understanding.explanation}</p>
                    <div :for={{finding, i} <- Enum.with_index(@cognitive_report.understanding.findings)}
                      id={"social-finding-#{i}"}
                      class="rounded border border-white/10 p-3">
                      <h4 class="font-medium">{finding.title}</h4>
                      <p class="mt-1 break-words text-sm text-zinc-300">{finding.detail}</p>
                      <p class="mt-1 text-xs text-zinc-500">Basis: {finding.basis} · {finding.level}</p>
                    </div>
                    <div :if={@cognitive_report.understanding.gaps != []} class="space-y-1">
                      <h4 class="text-sm font-semibold text-amber-200">Unresolved evidence</h4>
                      <p :for={gap <- @cognitive_report.understanding.gaps}
                         class="break-words text-sm text-zinc-300">• {gap}</p>
                    </div>
                  </section>
                  <section :if={Map.has_key?(@cognitive_report, :discourse)}
                    id="social-discourse" class="space-y-3 rounded-lg border border-white/10 bg-zinc-900/60 p-4 sm:p-5">
                    <h3 class="font-semibold text-cyan-200">COLONY-12 · Discourse evidence</h3>
                    <p class="text-sm text-zinc-400">
                      {@cognitive_report.discourse.status} ·
                      {length(@cognitive_report.discourse.interpretations)} supported sentence interpretations ·
                      {length(@cognitive_report.discourse.lexical_links)} unverified lexical links
                    </p>
                    <div :for={item <- @cognitive_report.discourse.interpretations}
                      class="space-y-1 rounded border border-white/10 p-3 text-sm">
                      <p class="font-semibold">{item.source_label} · {item.speaker} · {item.speech_act}</p>
                      <p class="whitespace-pre-wrap break-words text-zinc-300">{item.interpretation}</p>
                      <div :if={map_size(item.roles) > 0} class="flex flex-wrap gap-x-4 gap-y-1 text-xs text-zinc-300">
                        <span :for={{role, surface} <- Enum.sort(item.roles)} class="break-words">
                          <span class="font-semibold text-cyan-200">{role}:</span> {surface}
                        </span>
                      </div>
                      <div :if={is_map(item.topic_structure)}
                        class="rounded border border-amber-400/20 bg-amber-950/20 p-2 text-xs text-amber-100">
                        <p>Complement structure: <strong>{item.topic_structure.type}</strong> ·
                          {item.topic_structure.source}</p>
                        <p>Embedded clause: {item.topic_structure.embedded_clause_status} ·
                          {item.topic_structure.assertion_status}</p>
                        <p :if={item.topic_structure.type == :unparsed_topic}>
                          The topic is preserved as source wording only; its internal entities
                          and relationships have not been established.
                        </p>
                      </div>
                      <div :if={Map.get(item, :embedded_candidates, []) != []}
                        class="space-y-2 rounded border border-white/10 p-3 text-xs">
                        <p class="font-semibold text-cyan-200">Embedded grammatical evidence (not interpreted)</p>
                        <div :for={candidate <- item.embedded_candidates}
                          class="space-y-1 text-zinc-300">
                          <p>{candidate.construction} · {candidate.status}</p>
                          <p>Auxiliary: {candidate.auxiliary.surface} ·
                            predicate: {candidate.predicate.surface}</p>
                          <p>Subject binding: {candidate.subject_binding_status} ·
                            embedded clause: {candidate.embedded_clause_status} ·
                            {candidate.assertion_status}</p>
                          <div :if={is_map(Map.get(candidate, :subject_candidate))}
                            class="rounded border border-cyan-400/20 p-2">
                            <p>Adjacent grammatical subject candidate: {candidate.subject_candidate.surface}</p>
                            <p>{candidate.subject_candidate.source} ·
                              {candidate.subject_candidate.status}</p>
                            <p :if={Map.get(candidate.subject_candidate, :nominalization_status)}
                              class="text-amber-200">
                              Adjectival nominalization: {candidate.subject_candidate.nominalization_status} ·
                              adjective sense retained, not converted into a noun
                            </p>
                            <p>Attachment: {candidate.subject_candidate.attachment_status} ·
                              reference: {candidate.subject_candidate.coreference_status}</p>
                          </div>
                          <p :if={is_map(Map.get(candidate, :relation_diagnostic))}
                            class="text-amber-100">
                            Embedded relation evidence: {candidate.relation_diagnostic.status} ·
                            selected predicate sense:
                            {candidate.relation_diagnostic.selected_predicate_id || "none"}
                          </p>
                          <div :if={is_map(Map.get(candidate, :relation_candidate))}
                            class="space-y-1 rounded border border-cyan-400/20 bg-zinc-950/40 p-2">
                            <p class="font-semibold text-cyan-200">
                              Question-scoped structural relation candidate
                            </p>
                            <p class="break-words">
                              {candidate.relation_candidate.surface}
                            </p>
                            <p>Subject: {candidate.relation_candidate.subject.surface} ·
                              referent: {candidate.relation_candidate.subject.identity}</p>
                            <p>Predicate: {candidate.relation_candidate.predicate.surface} ·
                              lexical selection: {candidate.relation_candidate.predicate.selected_sense_id}</p>
                            <p>Attachment: {candidate.relation_candidate.attachment_status} ·
                              {candidate.relation_candidate.assertion_status} ·
                              remaining topic: {candidate.relation_candidate.remainder_status}</p>
                            <p class="text-amber-200">
                              This is a grammatical hypothesis inside a question, not an event or verified fact.
                            </p>
                          </div>
                          <p :if={is_map(Map.get(candidate, :qualification_diagnostic))}
                            class="text-amber-100">
                            Trailing qualification evidence:
                            {candidate.qualification_diagnostic.status} ·
                            qualifier verb:
                            {candidate.qualification_diagnostic.selected_qualifier_verb_id || "none"} ·
                            degree term:
                            {candidate.qualification_diagnostic.selected_degree_id || "none"}
                          </p>
                          <div :if={is_map(Map.get(candidate, :qualification_candidate))}
                            class="space-y-1 rounded border border-cyan-400/20 bg-zinc-950/40 p-2">
                            <p class="font-semibold text-cyan-200">
                              Question-scoped “not only” qualification candidate
                            </p>
                            <p class="break-words">{candidate.qualification_candidate.surface}</p>
                            <p>Focus marker: {candidate.qualification_candidate.focus_marker} ·
                              punctuation: {candidate.qualification_candidate.punctuation_bridge}</p>
                            <p>Qualifier: {candidate.qualification_candidate.qualifier_predicate.selected_sense_id} ·
                              degree term: {candidate.qualification_candidate.degree_term.selected_sense_id}</p>
                            <p>Comparison: {candidate.qualification_candidate.semantic_comparison_status} ·
                              {candidate.qualification_candidate.assertion_status}</p>
                            <p class="text-amber-200">
                              “Not only” is not a denial that the qualifier happened.
                              Coordination, comparison and shared-subject attachment remain unverified.
                            </p>
                          </div>
                          <p :if={is_nil(Map.get(candidate, :subject_candidate))}
                            class="text-zinc-400">
                            No independently resolved adjacent nominal candidate.
                          </p>
                          <div :if={is_map(Map.get(candidate, :subject_diagnostic))}
                            class="space-y-1 rounded border border-white/10 bg-zinc-950/40 p-2">
                            <p class="font-semibold text-amber-100">
                              Subject evidence: {candidate.subject_diagnostic.status}
                            </p>
                            <p :for={token <- candidate.subject_diagnostic.token_window}
                              class="break-words text-zinc-400">
                              Token {token.index}: {token.surface} · selected POS:
                              {token.selected_pos || "unknown"} · lexical hypothesis:
                              {token.selected_sense_id || "none"}
                            </p>
                            <p class="text-zinc-400">
                              Adjacent NP grammar signatures:
                              {length(candidate.subject_diagnostic.adjacent_noun_signatures)} ·
                              ATL noun constituents:
                              {length(candidate.subject_diagnostic.adjacent_atl_noun_phrases)}
                            </p>
                            <p class="text-zinc-400">
                              Adjectival nominalization signatures:
                              {length(Map.get(candidate.subject_diagnostic, :adjacent_nominalization_signatures, []))} ·
                              ATL nominalization constituents:
                              {length(Map.get(candidate.subject_diagnostic, :adjacent_atl_nominalizations, []))}
                            </p>
                            <p :for={nominal <- Map.get(candidate.subject_diagnostic, :adjacent_atl_nominalizations, [])}
                              class="break-words text-zinc-400">
                              ATL adjectival nominalization {inspect(nominal.span)} · {nominal.status} ·
                              unresolved roles: {inspect(nominal.unresolved_roles)}
                            </p>
                            <p :for={np <- candidate.subject_diagnostic.adjacent_atl_noun_phrases}
                              class="text-zinc-400">
                              ATL NP {inspect(np.span)} · {np.status} ·
                              unresolved roles: {inspect(np.unresolved_roles)}
                            </p>
                            <p class="text-amber-200">
                              Lexical selections remain hypotheses; no subject binding or factual assertion.
                            </p>
                          </div>
                          <p>Evidence: {Enum.join(Enum.map(candidate.grammatical_evidence, &to_string/1), ", ")}</p>
                        </div>
                      </div>
                      <p :if={item.unresolved_roles != []} class="text-xs text-amber-200">
                        Unresolved role grounding: {Enum.join(Enum.map(item.unresolved_roles, &to_string/1), ", ")}
                      </p>
                      <p class="text-xs text-amber-200">External, unverified · only this sentence is interpreted</p>
                    </div>
                    <p :if={@cognitive_report.discourse.interpretations == []} class="text-sm text-zinc-400">
                      No sentence has sufficient compositional evidence yet.
                    </p>
                    <div :for={link <- @cognitive_report.discourse.lexical_links}
                      class="text-sm text-zinc-300">
                      Sentence {link.from + 1} ↔ {link.to + 1} · shared terms:
                      {Enum.join(link.shared_terms, ", ")} · lexical overlap only, not verified coreference
                    </div>
                    <p :for={gap <- @cognitive_report.discourse.unresolved}
                      class="text-sm text-zinc-400">{gap}</p>
                  </section>
                  <article :for={{segment, index} <- Enum.with_index(@cognitive_report.segments)}
                    id={"social-analysis-segment-#{index}"}
                    class="min-w-0 space-y-3 rounded-lg border border-white/10 bg-zinc-900/70 p-4">
                    <h3 class="break-words font-semibold">{segment.label} · {segment.author}</h3>
                    <%= if segment.status == :analyzed do %>
                      <div class="grid gap-3 text-sm sm:grid-cols-2 lg:grid-cols-3">
                        <p>LIFG: <strong>{segment.result.lifg.status}</strong> · {segment.result.lifg.chosen_count} choices</p>
                        <p>ATL: <strong>{segment.result.atl.status}</strong> · {segment.result.atl.winner_count} winners</p>
                        <p>Sentence meaning: <strong>{segment.result.sentence_meaning.status}</strong></p>
                        <p :if={get_in(segment.result, [:sentence_meaning, :speech_act])}>
                          Speech act: <strong>{segment.result.sentence_meaning.speech_act}</strong>
                        </p>
                        <p :if={Map.has_key?(segment.result, :question_composition) and
                                   segment.result.question_composition.status != :no_question_signature}>
                          Question composition: <strong>{segment.result.question_composition.status}</strong>
                          <pre class="mt-2 max-w-full whitespace-pre-wrap break-all text-xs text-zinc-400">{inspect(Map.drop(segment.result.question_composition, [:status]), limit: :infinity, printable_limit: :infinity, charlists: :as_lists, pretty: true)}</pre>
                        </p>
                        <p :if={Map.has_key?(segment.result, :question_evidence)}>
                          Question frames: {segment.result.question_evidence.signature_count} · ATL resolved: {segment.result.question_evidence.resolved_constituents}
                        </p>
                        <p>Grammar: <strong>{segment.result.syntax.audit.outcome}</strong></p>
                        <p>Dictionary POS coverage: {segment.result.syntax.audit.pos_covered_tokens} / {segment.result.token_count} tokens</p>
                        <p>Lexical ambiguity: {if segment.result.acc_shadow.uncertain?, do: "flagged", else: "not flagged"}</p>
                      </div>
                      <div :if={get_in(segment.result, [:sentence_meaning, :definition])} class="rounded border border-cyan-400/20 p-3">
                        <h4 class="mb-1 text-sm font-semibold">Structural interpretation (not verified fact)</h4>
                        <p class="break-words text-sm text-zinc-300">{segment.result.sentence_meaning.definition}</p>
                      </div>
                      <div>
                        <h4 class="mb-1 text-sm font-semibold">Sentence signatures</h4>
                        <p class="break-words text-sm text-zinc-300">
                          {signature_labels(segment.result.syntax.signatures)}
                        </p>
                      </div>
                      <div :if={is_map(Map.get(segment.result, :declarative_evidence)) and
                                  Map.get(segment.result.declarative_evidence, :examined_frames, 0) > 0}
                        class="space-y-2 rounded border border-white/10 p-3">
                        <h4 class="text-sm font-semibold">COLONY-13 · Local declarative clause evidence</h4>
                        <p class="text-xs text-zinc-400">
                          {segment.result.declarative_evidence.examined_frames} grammar frames examined ·
                          {length(segment.result.declarative_evidence.candidates)} local candidates ·
                          not whole-sentence interpretation
                        </p>
                        <div :for={clause <- segment.result.declarative_evidence.candidates}
                          class="space-y-1 rounded border border-cyan-400/20 p-2 text-xs text-zinc-300">
                          <p class="font-semibold text-cyan-200">
                            {clause.source_frame} · {clause.status}
                          </p>
                          <p class="break-words">{clause.surface}</p>
                          <p>Subject: {clause.subject.surface} ·
                            {clause.subject.selected_sense_id}</p>
                          <p>Predicate: {clause.predicate.surface} ·
                            {clause.predicate.selected_sense_id}</p>
                          <p>Object: {clause.object.surface} ·
                            {clause.object.selected_sense_id}</p>
                          <p class="text-amber-200">
                            Clause attachment: {clause.clause_attachment} ·
                            polarity: {clause.polarity} · {clause.assertion_status} ·
                            {clause.verification}
                          </p>
                        </div>
                        <p :for={rejection <- segment.result.declarative_evidence.abstentions}
                          class="break-words text-xs text-zinc-400">
                          {rejection.frame} {inspect(rejection.span)} withheld:
                          {rejection.reason}
                        </p>
                      </div>
                      <div>
                        <h4 class="mb-1 text-sm font-semibold">Recognized topics (unverified)</h4>
                        <pre class="max-w-full whitespace-pre-wrap break-all text-xs text-zinc-300">{inspect(segment.result.comprehension.recognized_topics, limit: 8, printable_limit: 300)}</pre>
                      </div>
                      <div>
                        <h4 class="mb-1 text-sm font-semibold">Attributed proposition</h4>
                        <pre class="max-w-full whitespace-pre-wrap break-all text-xs text-zinc-300">{inspect(segment.result.reported_proposition, limit: 15, printable_limit: 500)}</pre>
                      </div>
                    <% else %>
                      <p class="text-sm text-amber-200">Probe unavailable: {inspect(segment.reason)}</p>
                    <% end %>
                  </article>
                <% else %>
                  <p class="text-sm text-amber-200">The cognitive report is unavailable.</p>
                <% end %>
              </div>
            </div>
          </div>
        </div>
      </section>
    </Layouts.app>
    """
  end

  defp fetch_topics(socket) do
    case SocialData.list_page(socket.assigns.page) do
      {:ok, data} ->
        assign(socket,
          topics: data.topics,
          has_next?: data.has_next?,
          loading?: false,
          error: nil
        )

      {:error, reason} ->
        assign(socket, topics: [], has_next?: false, loading?: false, error: error_message(reason))
    end
  rescue
    _ -> assign(socket, topics: [], has_next?: false, loading?: false, error: "Unable to load Colony topics.")
  end

  defp fetch_thread(socket) do
    case SocialData.thread(socket.assigns.post_id) do
      {:ok, data} ->
        assign(socket,
          topic: data.topic,
          comments: data.comments,
          comments_truncated?: data.comments_truncated?,
          loading?: false,
          error: nil
        )

      {:error, reason} ->
        assign(socket, topic: nil, comments: [], loading?: false, error: error_message(reason))
    end
  rescue
    _ -> assign(socket, topic: nil, comments: [], loading?: false, error: "Unable to load Colony conversation.")
  end

  defp signature_labels(signatures) do
    case signatures |> Enum.map(&to_string(&1.id)) |> Enum.join(", ") do
      "" -> "None detected"
      labels -> labels
    end
  end

  defp parse_page(page) when is_binary(page) do
    case Integer.parse(page) do
      {value, ""} when value in 1..4_001 -> value
      _ -> 1
    end
  end

  defp parse_page(_), do: 1

  defp preview(text) when is_binary(text), do: String.slice(text, 0, 260)
  defp preview(_), do: ""

  defp error_message(:colony_disabled), do: "Colony observation is disabled in the local configuration."
  defp error_message(:missing_colony_api_key), do: "No Colony API key is configured for the local server."
  defp error_message(:insecure_colony_key_file), do: "The local Colony API key file has unsafe permissions."
  defp error_message(:invalid_limit), do: "Colony max_posts must allow at least 25 topics per page."
  defp error_message(:rate_limited), do: "Colony is rate-limiting requests. Try refreshing later."
  defp error_message(:unauthorized), do: "Colony authentication was rejected."
  defp error_message(:forbidden), do: "Colony denied access to this conversation."
  defp error_message(:invalid_post_id), do: "Invalid Colony topic identifier."
  defp error_message(:colony_thread_identity_mismatch), do: "Colony returned a different topic identifier."
  defp error_message(:invalid_colony_feed), do: "Colony returned an unrecognized topics response."
  defp error_message(:invalid_colony_context), do: "Colony returned an unrecognized conversation response."
  defp error_message(:transport_error), do: "Could not contact Colony."
  defp error_message({:http_status, 404}), do: "This Colony topic was not found."
  defp error_message({:http_status, _}), do: "Colony could not complete the request."
  defp error_message(_), do: "Colony conversations are unavailable right now."
end
