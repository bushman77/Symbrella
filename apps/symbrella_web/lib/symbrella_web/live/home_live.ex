# apps/symbrella_web/lib/symbrella_web/live/home_live.ex
defmodule SymbrellaWeb.HomeLive do
  use SymbrellaWeb, :live_view

  alias SymbrellaWeb.ChatLive.HTML, as: ChatHTML
  alias SymbrellaWeb.ChatHistory
  alias Core.Response
  alias Core.Curiosity.EpisodeProbe
  alias Core.LexicalExplain
  import Ecto.Query, only: [from: 2]

  @choice_preview_limit 3
  @def_char_limit 120
  @senses_modal_limit 24

  # ─────────────────────────────────────────────────────────────────────────────
  # LiveView
  # ─────────────────────────────────────────────────────────────────────────────

  @impl true
  def mount(_params, _session, socket) do
    initial = initial_messages()

    initial_text_by_id =
      Map.new(initial, fn msg ->
        {msg.id, Map.get(msg, :explain_text, msg.text)}
      end)

    initial_explain_by_id =
      Map.new(initial, fn msg ->
        payload =
          Map.get(msg, :explain_payload) ||
            ChatHTML.explain_payload_for(%{
              id: msg.id,
              text: Map.get(msg, :explain_text, msg.text)
            })

        {msg.id, payload}
      end)

    {:ok,
     socket
     |> assign(
       page_title: "Chat",
       draft: "",
       bot_typing: false,
       pending_task: nil,
       cancelled_ref: nil,
       session_id: "s-" <> Integer.to_string(System.unique_integer([:positive])),
       explain_open?: false,
       explain_payload: %{},
       curiosity_idle_ref: nil,
       message_text_by_id: initial_text_by_id,
       explain_by_id: initial_explain_by_id
     )
     |> stream(:messages, initial)
     |> schedule_curiosity_idle()}
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
        msg_id = "u-" <> Integer.to_string(System.unique_integer([:positive]))

        msg = %{
          id: msg_id,
          role: :user,
          text: text
        }

        socket =
          socket
          |> cancel_curiosity_idle()
          |> stream_insert(:messages, msg)
          |> assign(
            draft: "",
            bot_typing: true,
            cancelled_ref: nil,
            message_text_by_id: Map.put(socket.assigns.message_text_by_id, msg_id, text)
          )
          |> push_event("chat:clear-input", %{})
          |> push_event("chat:scroll", %{to: "composer"})

        ChatHistory.append(msg)

        task =
          Task.Supervisor.async_nolink(Symbrella.TaskSup, fn ->
            build_planned_reply(text, socket.assigns.session_id)
          end)

        {:noreply, assign(socket, :pending_task, task)}
    end
  end

  @impl true
  def handle_event("explain_open", %{"id" => id} = params, socket) do
    text =
      case params do
        %{"text" => t} when is_binary(t) -> t
        _ -> Map.get(socket.assigns.message_text_by_id, id, "")
      end

    payload =
      socket.assigns.explain_by_id[id] ||
        ChatHTML.explain_payload_for(%{id: id, text: text})

    {:noreply, assign(socket, explain_open?: true, explain_payload: payload)}
  end

  @impl true
  def handle_event("explain_close", _params, socket) do
    {:noreply, assign(socket, explain_open?: false, explain_payload: %{})}
  end

  @impl true
  def handle_event("stop", _params, socket) do
    case socket.assigns.pending_task do
      %Task{} = task ->
        _ = Task.shutdown(task, :brutal_kill)
        Process.demonitor(task.ref, [:flush])

        bot_id = "x-" <> Integer.to_string(System.unique_integer([:positive]))
        bot_text = "(stopped)"

        payload = ChatHTML.explain_payload_for(%{id: bot_id, text: bot_text})
        stopped = %{id: bot_id, role: :assistant, text: bot_text}

        ChatHistory.append(stopped)

        {:noreply,
         socket
         |> assign(
           bot_typing: false,
           cancelled_ref: task.ref,
           pending_task: nil,
           message_text_by_id: Map.put(socket.assigns.message_text_by_id, bot_id, bot_text),
           explain_by_id: Map.put(socket.assigns.explain_by_id, bot_id, payload)
         )
         |> stream_insert(:messages, stopped)
         |> push_event("chat:scroll", %{to: "composer"})}

      _ ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_info({ref, payload}, %{assigns: %{pending_task: %Task{ref: ref}}} = socket) do
    Process.demonitor(ref, [:flush])

    reply = to_bot_reply(payload)

    reply_text = reply.text
    tone = Map.get(reply, :tone)
    meta = Map.get(reply, :meta)
    si = Map.get(reply, :si, %{})

    explain_text = Map.get(reply, :explain_text, reply_text)
    senses_selected = Map.get(reply, :senses_selected, [])

    intent = Map.get(si, :intent)
    confidence = Map.get(si, :confidence)

    bot_id = "b-" <> Integer.to_string(System.unique_integer([:positive]))

    explain_payload =
      ChatHTML.explain_payload_for(%{
        id: bot_id,
        text: explain_text,
        tone: tone,
        intent: intent,
        confidence: confidence,
        from: meta || %{},
        senses_selected: senses_selected
      })

    bot = %{
      id: bot_id,
      role: :assistant,
      text: reply_text,
      tone: tone,
      meta: meta,
      explain_text: explain_text,
      explain_payload: explain_payload
    }

    ChatHistory.append(bot)

    {:noreply,
     socket
     |> assign(
       bot_typing: false,
       pending_task: nil,
       message_text_by_id: Map.put(socket.assigns.message_text_by_id, bot_id, explain_text),
       explain_by_id: Map.put(socket.assigns.explain_by_id, bot_id, explain_payload)
     )
     |> stream_insert(:messages, bot)
     |> push_event("chat:scroll", %{to: "composer"})
     |> schedule_curiosity_idle()}
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
        bot_id = "e-" <> Integer.to_string(System.unique_integer([:positive]))
        bot_text = "Oops—my brain hit a snag: #{Exception.format_exit(reason)}"

        payload = ChatHTML.explain_payload_for(%{id: bot_id, text: bot_text})

        msg = %{
          id: bot_id,
          role: :assistant,
          text: bot_text
        }

        ChatHistory.append(msg)

        {:noreply,
         socket
         |> assign(
           bot_typing: false,
           pending_task: nil,
           message_text_by_id: Map.put(socket.assigns.message_text_by_id, bot_id, bot_text),
           explain_by_id: Map.put(socket.assigns.explain_by_id, bot_id, payload)
         )
         |> stream_insert(:messages, msg)
         |> push_event("chat:scroll", %{to: "composer"})
         |> schedule_curiosity_idle()}

      true ->
        {:noreply, socket}
    end
  end

  @impl true
  def handle_info({:curiosity_idle, ref}, socket) do
    cond do
      not match?({^ref, _timer_ref}, socket.assigns.curiosity_idle_ref) ->
        {:noreply, socket}

      socket.assigns.bot_typing or match?(%Task{}, socket.assigns.pending_task) ->
        {:noreply, schedule_curiosity_idle(socket)}

      true ->
        socket = assign(socket, :curiosity_idle_ref, nil)

        case EpisodeProbe.idle_question(socket.assigns.session_id) do
          {:ok, question, meta} ->
            bot_id = "c-" <> Integer.to_string(System.unique_integer([:positive]))

            explain_payload =
              ChatHTML.explain_payload_for(%{
                id: bot_id,
                text: question,
                tone: :curious,
                intent: :curiosity,
                confidence: meta[:uncertainty],
                from: meta
              })

            bot = %{
              id: bot_id,
              role: :assistant,
              text: question,
              tone: :curious,
              meta: meta,
              explain_text: question,
              explain_payload: explain_payload
            }

            ChatHistory.append(bot)

            {:noreply,
             socket
             |> assign(
               message_text_by_id: Map.put(socket.assigns.message_text_by_id, bot_id, question),
               explain_by_id: Map.put(socket.assigns.explain_by_id, bot_id, explain_payload)
             )
             |> stream_insert(:messages, bot)
             |> push_event("chat:scroll", %{to: "composer"})
             |> schedule_curiosity_idle()}

          :none ->
            {:noreply, schedule_curiosity_idle(socket)}
        end
    end
  end

  @impl true
  def render(assigns), do: ChatHTML.chat(assigns)

  defp initial_messages do
    case ChatHistory.list() do
      [] -> [%{id: "m1", role: :assistant, text: "Welcome to Symbrella chat  👋"}]
      messages -> messages
    end
  end

  defp schedule_curiosity_idle(socket) do
    socket = cancel_curiosity_idle(socket)

    if connected?(socket) and curiosity_idle_enabled?() and curiosity_idle_ms() > 0 do
      ref = make_ref()
      timer_ref = Process.send_after(self(), {:curiosity_idle, ref}, curiosity_idle_ms())
      assign(socket, :curiosity_idle_ref, {ref, timer_ref})
    else
      assign(socket, :curiosity_idle_ref, nil)
    end
  end

  defp cancel_curiosity_idle(socket) do
    case Map.get(socket.assigns, :curiosity_idle_ref) do
      nil -> socket
      {_ref, timer_ref} -> Process.cancel_timer(timer_ref)
      timer_ref when is_reference(timer_ref) -> Process.cancel_timer(timer_ref)
    end

    assign(socket, :curiosity_idle_ref, nil)
  end

  defp curiosity_idle_ms do
    home_live_config()
    |> Keyword.get(:curiosity_idle_ms, 180_000)
    |> case do
      value when is_integer(value) and value >= 0 -> value
      _ -> 180_000
    end
  end

  defp curiosity_idle_enabled? do
    home_live_config()
    |> Keyword.get(:curiosity_idle_enabled?, false)
    |> Kernel.==(true)
  end

  defp home_live_config do
    Application.get_env(:symbrella_web, __MODULE__, [])
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Planner integration
  # ─────────────────────────────────────────────────────────────────────────────
  defp build_planned_reply(user_text, session_id) do
    memory_si_like = %{
      session_id: session_id,
      intent: :unknown,
      confidence: 1.0,
      text: user_text,
      source: :user
    }

    memory_reply =
      if Code.ensure_loaded?(Response) and function_exported?(Response, :memory_reply, 1) do
        Response.memory_reply(memory_si_like)
      end

    case memory_reply do
      {tone, reply_text, meta} ->
        %{
          text: reply_text,
          tone: tone,
          meta: meta,
          si: memory_si_like,
          senses_selected: [],
          explain_text: reply_text
        }

      nil ->
        build_semantic_reply(user_text, session_id)
    end
  end

  defp build_semantic_reply(user_text, session_id) do
    si =
      Core.resolve_input(user_text,
        mode: :prod,
        enrich_lexicon?: true,
        lexicon_stage?: true,
        session_id: session_id
      )
      |> Map.put(:session_id, session_id)

    lexical_tail = build_lexical_tail(si)
    mood = mood_from_si(si)

    si_like = %{
      session_id: session_id,
      intent: Map.get(si, :intent, :unknown),
      confidence: Map.get(si, :confidence, 0.0),
      keyword: Map.get(si, :keyword, ""),
      text: user_text,
      tokens: Map.get(si, :tokens, []),
      source: Map.get(si, :source, :user)
    }

    # Prefer the response already attached by Core.resolve_input/2; otherwise fall back to planner.
    {tone, reply_text, meta} =
      cond do
        is_binary(Map.get(si, :response_text)) and Map.get(si, :response_text) != "" ->
          {
            Map.get(si, :response_tone, :warm),
            Map.get(si, :response_text),
            Map.get(si, :response_meta, %{})
          }

        Code.ensure_loaded?(Response) and function_exported?(Response, :plan, 2) ->
          Response.plan(si_like, mood)

        true ->
          {nil, format_si_reply(si), %{}}
      end

    visible = reply_text
    senses_selected = build_senses_selected(si, visible)

    explain_text =
      join_blocks([
        visible,
        lexical_tail,
        get_in(meta || %{}, [:explanation, :text])
      ])

    %{
      text: visible,
      tone: tone,
      meta: meta,
      si: si,
      senses_selected: senses_selected,
      explain_text: explain_text
    }
  end

  defp build_lexical_tail(si) do
    tokens = Map.get(si, :tokens)
    token_structs = Map.get(si, :token_structs)

    token_count =
      cond do
        is_list(tokens) and tokens != [] -> length(tokens)
        is_list(token_structs) -> length(token_structs)
        true -> 0
      end

    max_items =
      cond do
        token_count <= 0 -> 3
        token_count <= 4 -> token_count
        true -> 3
      end

    if max_items > 0 and
         Code.ensure_loaded?(LexicalExplain) and
         function_exported?(LexicalExplain, :from_si, 2) do
      LexicalExplain.from_si(si, max_items: max_items)
    else
      ""
    end
  end

  defp mood_from_si(si) when is_map(si) do
    case Map.get(si, :mood) do
      %{} = m -> m
      _ -> %{}
    end
  end

  defp join_blocks(blocks) when is_list(blocks) do
    blocks
    |> Enum.map(fn
      nil -> ""
      v -> to_string(v)
    end)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.join("\n\n")
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Senses Selected builder (NO Db calls; respects db <- brain <- core <- web)
  # ─────────────────────────────────────────────────────────────────────────────

  defp build_senses_selected(%{} = si, reply_text) do
    case phrase_reference_sense(si, reply_text) do
      nil -> build_token_senses_selected(si)
      phrase_item -> [phrase_item]
    end
  end

  defp build_token_senses_selected(%{} = si) do
    raw_choices = Map.get(si, :lifg_choices, []) || []
    cells = si_cells(si) ++ candidate_cells_from_si(si) ++ db_cells_for_choices(raw_choices)
    sense_glossary = build_sense_glossary(si, raw_choices, cells)
    choices = explainable_choices(raw_choices, cells, sense_glossary)

    {items, _seen} =
      Enum.reduce(choices, {[], MapSet.new()}, fn ch, {acc, seen} ->
        tok_idx = ch[:token_index] || ch[:tok_index] || ch[:index]

        id = ch[:id] || ch[:chosen_id]
        pos = display_pos(ch)
        src = ch[:src] || ch[:source]
        score = ch[:score]

        lemma = ch[:lemma] || ch[:token] || ch[:surface] || ""

        surface =
          ch[:surface] ||
            ch[:raw_phrase] ||
            ch[:token_phrase] ||
            lemma

        alt_ids = List.wrap(ch[:alt_ids])
        key = {tok_idx, id, pos, src, surface, lemma}

        if MapSet.member?(seen, key) do
          {acc, seen}
        else
          label =
            [
              safe_str(surface),
              " → ",
              safe_str(lemma),
              " pos=",
              safe_str(pos),
              " src=",
              safe_str(src),
              " score=",
              fmt_score(score)
            ]
            |> IO.iodata_to_binary()
            |> String.trim()

          defn =
            first_present([
              contextual_definition_for_choice(ch),
              definition_for_choice(ch, cells),
              candidate_definition_from_si(si, ch),
              glossary_field(sense_glossary, ch, :definition),
              generated_definition(ch)
            ])

          ex =
            first_present([
              example_for_choice(ch, cells),
              candidate_example_from_si(si, ch),
              glossary_field(sense_glossary, ch, :example)
            ])

          item = %{
            token_index: tok_idx,
            id: id,
            alt_ids: alt_ids,
            label: label,
            definition: defn,
            example: ex
          }

          {[item | acc], MapSet.put(seen, key)}
        end
      end)

    items
    |> Enum.reverse()
    |> Enum.take(@senses_modal_limit)
  end

  defp explainable_choices(choices, cells, sense_glossary) when is_list(choices) do
    choices
    |> Enum.reject(&suppress_explain_choice?(&1, cells, sense_glossary))
  end

  defp explainable_choices(_, _, _), do: []

  defp suppress_explain_choice?(choice, cells, sense_glossary) when is_map(choice) do
    generated_only? = stored_definition_for_choice(choice, cells, sense_glossary) == ""

    (phrase_choice?(choice) and low_confidence_choice?(choice) and generated_only?) or
      (closed_class_choice?(choice) and generated_only?)
  end

  defp suppress_explain_choice?(_, _, _), do: true

  defp stored_definition_for_choice(choice, cells, sense_glossary) do
    first_present([
      definition_for_choice(choice, cells),
      glossary_field(sense_glossary, choice, :definition)
    ])
  end

  defp phrase_reference_sense(%{} = si, reply_text) do
    sentence = Map.get(si, :sentence) || Map.get(si, "sentence") || ""
    phrase = sentence |> to_string() |> String.replace(~r/\s+/u, " ") |> String.trim()

    cond do
      phrase == "" ->
        nil

      reference_reply?(reply_text) ->
        %{
          token_index: 0,
          id: "#{String.downcase(phrase)}|phrase|reference",
          alt_ids: [],
          label: "#{phrase} → #{phrase} pos=phrase src=reply-reference score=1.00",
          definition: reference_definition(phrase, reply_text),
          example: ""
        }

      true ->
        nil
    end
  end

  defp phrase_reference_sense(_, _), do: nil

  defp reference_reply?(reply_text) when is_binary(reply_text) do
    text = String.downcase(reply_text)

    String.contains?(text, "reference to") or
      String.contains?(text, "film") or
      String.contains?(text, "movie") or
      String.contains?(text, "book") or
      String.contains?(text, "song") or
      String.contains?(text, "album") or
      String.contains?(text, "title")
  end

  defp reference_reply?(_), do: false

  defp reference_definition(phrase, reply_text) do
    kind =
      reply_text
      |> to_string()
      |> String.downcase()
      |> cond_kind()

    "Recognized as a #{kind} reference/title phrase: \"#{phrase}\". Interpreted as one named reference, not as separate word senses."
  end

  defp cond_kind(text) do
    cond do
      String.contains?(text, "film") or String.contains?(text, "movie") -> "film"
      String.contains?(text, "book") -> "book"
      String.contains?(text, "song") -> "song"
      String.contains?(text, "album") -> "album"
      true -> "cultural"
    end
  end

  defp si_cells(si) do
    Map.get(si, :active_cells, Map.get(si, :cells, [])) || []
  end

  defp candidate_cells_from_si(%{} = si) do
    si
    |> sense_candidate_values()
    |> Enum.map(&candidate_to_cell/1)
    |> Enum.reject(&(&1 == %{}))
  end

  defp sense_candidate_values(%{} = si) do
    case Map.get(si, :sense_candidates) || Map.get(si, "sense_candidates") || %{} do
      %{} = sc ->
        sc
        |> Enum.flat_map(fn
          {_key, values} when is_list(values) -> values
          {_key, value} -> List.wrap(value)
        end)
        |> Enum.filter(&is_map/1)

      values when is_list(values) ->
        Enum.filter(values, &is_map/1)

      _ ->
        []
    end
  end

  defp candidate_to_cell(candidate) when is_map(candidate) do
    id = cand_get(candidate, :id) || cand_get(candidate, :chosen_id)
    norm = cand_get(candidate, :norm) || cand_get(candidate, :lemma) || cand_get(candidate, :word)

    %{}
    |> maybe_put_nonblank(:id, id)
    |> maybe_put_nonblank(
      :word,
      cand_get(candidate, :word) || cand_get(candidate, :surface) || norm
    )
    |> maybe_put_nonblank(:norm, norm || id_norm(id))
    |> maybe_put_nonblank(:pos, cand_get(candidate, :pos) || pos_from_id(id))
    |> maybe_put_nonblank(:definition, candidate_definition(candidate))
    |> maybe_put_nonblank(:example, candidate_example(candidate))
  end

  defp candidate_to_cell(_), do: %{}

  # ─────────────────────────────────────────────────────────────────────────────
  # Candidate field lookups from si.sense_candidates
  # ─────────────────────────────────────────────────────────────────────────────

  defp candidate_definition_from_si(%{} = si, choice),
    do: candidate_field_from_si(si, choice, [:definition, :def, :gloss, :meaning])

  defp candidate_definition_from_si(_, _), do: ""

  defp candidate_example_from_si(%{} = si, choice),
    do: candidate_field_from_si(si, choice, [:example, :ex, :usage, :sample])

  defp candidate_example_from_si(_, _), do: ""

  defp candidate_field_from_si(%{} = si, choice, keys) when is_list(keys) do
    sc = Map.get(si, :sense_candidates) || Map.get(si, "sense_candidates") || %{}

    ids = compatible_choice_ids(choice)
    tok_idx = choice[:token_index] || choice[:tok_index] || choice[:index]
    choice_norms = choice_norms(choice)

    by_tok =
      case tok_idx do
        nil ->
          []

        i ->
          Map.get(sc, i) ||
            Map.get(sc, Integer.to_string(i)) ||
            Map.get(sc, to_string(i)) ||
            []
      end

    all_lists =
      if is_list(by_tok) and by_tok != [] do
        [by_tok]
      else
        sc
        |> Enum.flat_map(fn
          {_k, v} when is_list(v) -> [v]
          _ -> []
        end)
      end

    cand =
      all_lists
      |> Enum.flat_map(fn v -> if is_list(v), do: v, else: [] end)
      |> Enum.find(fn c ->
        cand_id = cand_get(c, :id) || cand_get(c, :chosen_id)
        cand_norm = cand_get(c, :norm) || cand_get(c, :lemma) || cand_get(c, :word)

        (is_binary(cand_id) and cand_id in ids) ||
          norm_text(cand_norm || "") in choice_norms
      end)

    keys
    |> Enum.find_value(fn k -> cand_get(cand, k) end)
    |> Kernel.||("")
    |> to_string()
    |> gloss()
  end

  defp cand_get(map, key) when is_map(map) and is_atom(key) do
    Map.get(map, key) || Map.get(map, Atom.to_string(key))
  end

  defp cand_get(map, key) when is_map(map), do: Map.get(map, key)
  defp cand_get(_, _), do: nil

  defp candidate_definition(candidate) when is_map(candidate) do
    candidate
    |> candidate_field([:definition, :def, :gloss, :meaning])
    |> gloss()
  end

  defp candidate_example(candidate) when is_map(candidate) do
    candidate
    |> candidate_field([:example, :ex, :usage, :sample])
    |> gloss()
  end

  defp candidate_field(candidate, keys) when is_map(candidate) do
    Enum.find_value(keys, fn key -> cand_get(candidate, key) end)
  end

  defp display_pos(choice) when is_map(choice) do
    choice[:pos] ||
      choice[:chosen_pos] ||
      contextual_pos_for_choice(choice)
  end

  defp display_pos(_), do: nil

  defp contextual_definition_for_choice(choice) when is_map(choice) do
    case primary_choice_norm(choice) do
      "why" ->
        "Interrogative word used to ask for a reason or cause."

      "do" ->
        "Auxiliary verb used to form a question, negation, or emphasis."

      "does" ->
        "Auxiliary verb used to form a question, negation, or emphasis."

      "did" ->
        "Auxiliary verb used to form a question, negation, or emphasis."

      "bury" ->
        "To put, hide, or cover something under earth or another material."

      "buries" ->
        "To put, hide, or cover something under earth or another material."

      "buried" ->
        "Put, hidden, or covered under earth or another material."

      "poop" ->
        "Feces, or to defecate; the everyday bodily-waste sense."

      "poops" ->
        "Feces, or defecates; the everyday bodily-waste sense."

      _ ->
        ""
    end
  end

  defp contextual_definition_for_choice(_), do: ""

  defp contextual_pos_for_choice(choice) when is_map(choice) do
    case primary_choice_norm(choice) do
      "why" -> "interrogative"
      norm when norm in ["do", "does", "did"] -> "aux"
      norm when norm in ["bury", "buries", "buried"] -> "verb"
      norm when norm in ["poop", "poops"] -> "noun/verb"
      _ -> nil
    end
  end

  defp primary_choice_norm(choice) when is_map(choice) do
    choice
    |> choice_norms()
    |> List.first()
    |> Kernel.||("")
  end

  defp phrase_choice?(choice) when is_map(choice) do
    id = choice[:id] || choice[:chosen_id]
    pos = safe_str(choice[:pos] || choice[:chosen_pos])

    phrase_text =
      choice[:surface] ||
        choice[:raw_phrase] ||
        choice[:token_phrase] ||
        choice[:lemma] ||
        choice[:token]

    String.downcase(pos) == "phrase" or
      (is_binary(id) and String.contains?(id, "|phrase")) or
      word_count(phrase_text) > 1
  end

  defp low_confidence_choice?(choice) when is_map(choice) do
    case numeric_score(choice[:score]) do
      nil -> false
      score -> score < 0.5
    end
  end

  defp closed_class_choice?(choice) when is_map(choice) do
    pos =
      choice[:pos] ||
        choice[:chosen_pos] ||
        pos_from_id(choice[:id] || choice[:chosen_id])

    norm =
      choice
      |> choice_norms()
      |> List.first()

    safe_str(pos) in ["det", "determiner", "article", "prep", "preposition", "conj", "particle"] or
      norm in ["the", "a", "an", "of", "to", "in", "on", "at", "by", "for", "and", "or"]
  end

  defp compatible_choice_ids(choice) when is_map(choice) do
    choice_norms = choice_norms(choice)

    [choice[:id], choice[:chosen_id] | List.wrap(choice[:alt_ids])]
    |> Enum.filter(&is_binary/1)
    |> Enum.filter(fn id ->
      idn = norm_text(id_norm(id))

      idn == "" or
        idn in choice_norms or
        phrase_id_compatible?(id, choice_norms)
    end)
    |> Enum.uniq()
  end

  defp compatible_choice_ids(_), do: []

  defp phrase_id_compatible?(id, choice_norms) when is_binary(id) do
    String.contains?(id, "|phrase") and norm_text(id_norm(id)) in choice_norms
  end

  defp phrase_id_compatible?(_, _), do: false

  defp choice_norms(choice) when is_map(choice) do
    norms =
      [
        choice[:norm],
        choice[:lemma],
        choice[:token],
        choice[:surface],
        choice[:raw_phrase],
        choice[:token_phrase]
      ]
      |> Enum.map(&norm_text/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    if norms == [] do
      (choice[:id] || choice[:chosen_id])
      |> id_norm()
      |> norm_text()
      |> case do
        "" -> []
        norm -> [norm]
      end
    else
      norms
    end
  end

  defp choice_norms(_), do: []

  defp word_count(value) do
    value
    |> norm_text()
    |> case do
      "" -> 0
      text -> text |> String.split(" ") |> length()
    end
  end

  defp numeric_score(nil), do: nil
  defp numeric_score(score) when is_number(score), do: score

  defp numeric_score(score) when is_binary(score) do
    case Float.parse(String.trim(score)) do
      {parsed, _} -> parsed
      _ -> nil
    end
  end

  defp numeric_score(_), do: nil

  # ─────────────────────────────────────────────────────────────────────────────
  # Definition/example resolution (cells + choice + alt_ids)
  # ─────────────────────────────────────────────────────────────────────────────

  defp definition_for_choice(choice, cells) do
    by_id = index_cells_by_id(cells)
    by_norm = index_cells_by_norm(cells)

    id = choice[:id] || choice[:chosen_id]
    ids = compatible_choice_ids(choice)
    choice_norms = choice_norms(choice)

    direct =
      choice[:definition] ||
        choice[:def] ||
        choice[:gloss] ||
        choice[:meaning]

    with_direct = gloss(direct)

    with_choice =
      if id in ids do
        by_id
        |> Map.get(id)
        |> cell_def()
        |> gloss()
      else
        ""
      end

    cond do
      with_direct != "" ->
        with_direct

      with_choice != "" ->
        with_choice

      true ->
        with_alt = first_def_from_ids(ids -- [id], by_id)

        cond do
          with_alt != "" ->
            with_alt

          true ->
            first_def_from_norms(choice_norms, by_norm)
        end
    end
  end

  defp db_cells_for_choices(choices) when is_list(choices) do
    if Code.ensure_loaded?(Db) and Code.ensure_loaded?(Db.BrainCell) do
      ids =
        choices
        |> Enum.flat_map(&compatible_choice_ids/1)
        |> Enum.reject(&String.contains?(&1, "|fallback"))
        |> Enum.uniq()

      norms =
        choices
        |> Enum.flat_map(&choice_norms/1)
        |> Enum.reject(&(&1 == ""))
        |> Enum.uniq()

      if ids == [] and norms == [] do
        []
      else
        try do
          from(c in Db.BrainCell,
            where:
              (not is_nil(c.id) and c.id in ^ids) or
                (not is_nil(c.norm) and c.norm in ^norms),
            select: %{
              id: c.id,
              word: c.word,
              norm: c.norm,
              pos: c.pos,
              definition: c.definition,
              example: c.example
            },
            limit: 50
          )
          |> Db.all()
        rescue
          _ -> []
        catch
          _, _ -> []
        end
      end
    else
      []
    end
  end

  defp db_cells_for_choices(_), do: []

  defp build_sense_glossary(%{} = si, choices, cells) do
    sense_candidate_values(si)
    |> Kernel.++(List.wrap(choices))
    |> Kernel.++(List.wrap(cells))
    |> Enum.reduce(%{}, fn source, acc ->
      defn = source_definition(source)
      ex = source_example(source)

      if defn == "" and ex == "" do
        acc
      else
        source
        |> source_keys()
        |> Enum.reduce(acc, fn key, acc2 ->
          Map.update(acc2, key, %{definition: defn, example: ex}, fn old ->
            %{
              definition: first_present([old.definition, defn]),
              example: first_present([old.example, ex])
            }
          end)
        end)
      end
    end)
  end

  defp build_sense_glossary(_, _, _), do: %{}

  defp glossary_field(glossary, choice, field) when is_map(glossary) and is_map(choice) do
    choice
    |> choice_lookup_keys()
    |> Enum.find_value("", fn key ->
      case Map.get(glossary, key) do
        %{^field => value} when is_binary(value) and value != "" -> value
        _ -> nil
      end
    end)
  end

  defp glossary_field(_, _, _), do: ""

  defp source_keys(source) when is_map(source) do
    id = cand_get(source, :id) || cand_get(source, :chosen_id)
    alt_ids = List.wrap(cand_get(source, :alt_ids))

    norm =
      cand_get(source, :norm) ||
        cand_get(source, :lemma) ||
        cand_get(source, :token) ||
        cand_get(source, :surface) ||
        cand_get(source, :word) ||
        id_norm(id)

    ([id | alt_ids]
     |> Enum.filter(&is_binary/1)
     |> Enum.map(&{:id, &1})) ++
      case norm_text(norm) do
        "" -> []
        n -> [{:norm, n}]
      end
  end

  defp source_keys(_), do: []

  defp choice_lookup_keys(choice) when is_map(choice) do
    id_keys =
      choice
      |> compatible_choice_ids()
      |> Enum.map(&{:id, &1})

    norm_keys =
      choice
      |> choice_norms()
      |> Enum.map(&{:norm, &1})

    id_keys ++ norm_keys
  end

  defp source_definition(source) when is_map(source) do
    source
    |> candidate_field([:definition, :def, :gloss, :meaning])
    |> gloss()
  end

  defp source_definition(_), do: ""

  defp source_example(source) when is_map(source) do
    source
    |> candidate_field([:example, :ex, :usage, :sample])
    |> gloss()
  end

  defp source_example(_), do: ""

  defp generated_definition(choice) when is_map(choice) do
    lemma =
      choice[:lemma] ||
        choice[:token] ||
        choice[:surface] ||
        id_norm(choice[:id] || choice[:chosen_id])

    pos = choice[:pos] || choice[:chosen_pos] || pos_from_id(choice[:id] || choice[:chosen_id])
    lemma = norm_text(lemma)
    pos = safe_str(pos)

    cond do
      lemma == "" ->
        "Selected sense; no stored dictionary gloss is attached yet."

      pos != "" ->
        "Selected #{pos} sense for \"#{lemma}\"; no stored dictionary gloss is attached yet."

      true ->
        "Selected sense for \"#{lemma}\"; no stored dictionary gloss is attached yet."
    end
  end

  defp generated_definition(_), do: "Selected sense; no stored dictionary gloss is attached yet."

  defp example_for_choice(choice, cells) do
    by_id = index_cells_by_id(cells)
    by_norm = index_cells_by_norm(cells)

    id = choice[:id] || choice[:chosen_id]
    ids = compatible_choice_ids(choice)
    choice_norms = choice_norms(choice)

    direct =
      choice[:example] ||
        choice[:ex] ||
        choice[:usage] ||
        choice[:sample]

    with_direct = gloss(direct)

    with_choice =
      if id in ids do
        by_id
        |> Map.get(id)
        |> cell_ex()
        |> gloss()
      else
        ""
      end

    cond do
      with_direct != "" ->
        with_direct

      with_choice != "" ->
        with_choice

      true ->
        with_alt = first_ex_from_ids(ids -- [id], by_id)

        cond do
          with_alt != "" ->
            with_alt

          true ->
            first_ex_from_norms(choice_norms, by_norm)
        end
    end
  end

  defp first_def_from_ids([], _by_id), do: ""

  defp first_def_from_ids([h | t], by_id) do
    case by_id |> Map.get(h) |> cell_def() |> gloss() do
      "" -> first_def_from_ids(t, by_id)
      d -> d
    end
  end

  defp first_def_from_norms([], _by_norm), do: ""

  defp first_def_from_norms([h | t], by_norm) do
    case by_norm |> Map.get(h) |> cell_def() |> gloss() do
      "" -> first_def_from_norms(t, by_norm)
      d -> d
    end
  end

  defp first_ex_from_ids([], _by_id), do: ""

  defp first_ex_from_ids([h | t], by_id) do
    case by_id |> Map.get(h) |> cell_ex() |> gloss() do
      "" -> first_ex_from_ids(t, by_id)
      e -> e
    end
  end

  defp first_ex_from_norms([], _by_norm), do: ""

  defp first_ex_from_norms([h | t], by_norm) do
    case by_norm |> Map.get(h) |> cell_ex() |> gloss() do
      "" -> first_ex_from_norms(t, by_norm)
      e -> e
    end
  end

  defp index_cells_by_id(cells) do
    Enum.reduce(cells || [], %{}, fn c, acc ->
      case cell_id(c) do
        nil -> acc
        id -> Map.put(acc, id, c)
      end
    end)
  end

  defp index_cells_by_norm(cells) do
    Enum.reduce(cells || [], %{}, fn c, acc ->
      case cell_norm(c) do
        nil -> acc
        n -> Map.put_new(acc, n, c)
      end
    end)
  end

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

  defp pos_from_id(id) when is_binary(id) do
    case String.split(id, "|") do
      [_lemma, pos | _] -> pos
      _ -> nil
    end
  end

  defp pos_from_id(_), do: nil

  defp cell_def(c) when is_map(c) do
    Map.get(c, :definition) ||
      Map.get(c, "definition") ||
      Map.get(c, :def) ||
      Map.get(c, "def") ||
      Map.get(c, :gloss) ||
      Map.get(c, "gloss")
  end

  defp cell_def(_), do: nil

  defp cell_ex(c) when is_map(c) do
    Map.get(c, :example) ||
      Map.get(c, "example") ||
      Map.get(c, :ex) ||
      Map.get(c, "ex") ||
      Map.get(c, :usage) ||
      Map.get(c, "usage") ||
      Map.get(c, :sample) ||
      Map.get(c, "sample")
  end

  defp cell_ex(_), do: nil

  # ─────────────────────────────────────────────────────────────────────────────
  # Formatting / debug
  # ─────────────────────────────────────────────────────────────────────────────

  defp format_si_reply(si) do
    tokens = Map.get(si, :tokens, [])
    cells = si_cells(si)
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
        " · src=#{inspect(Map.get(si, :source))}" <>
        if(token_preview != "", do: " · [#{token_preview}]", else: "")

    if choices_preview == "" do
      base
    else
      base <> " · choices: [#{choices_preview}]"
    end
  end

  defp format_choice_with_def(choice, cells) do
    lemma = choice[:lemma] || choice[:token] || ""
    id = choice[:id] || choice[:chosen_id]
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

  # ─────────────────────────────────────────────────────────────────────────────
  # General utilities
  # ─────────────────────────────────────────────────────────────────────────────

  defp sanitize_user_text(text) do
    text
    |> to_string()
    |> String.replace(~r/\r\n?/, "\n")
    |> String.replace(~r/[ \t]+(\n)/, "\\1")
    |> String.replace(~r/\n{3,}/, "\n\n")
    |> String.trim()
  end

  defp to_bot_reply({_, %{text: _} = reply}), do: reply
  defp to_bot_reply(%{text: _} = reply), do: reply
  defp to_bot_reply(text) when is_binary(text), do: %{text: text}
  defp to_bot_reply(other), do: %{text: inspect(other)}

  defp safe_str(nil), do: ""
  defp safe_str(v) when is_atom(v), do: Atom.to_string(v)
  defp safe_str(v) when is_binary(v), do: v
  defp safe_str(v), do: to_string(v)

  defp gloss(nil), do: ""
  defp gloss(""), do: ""

  defp gloss(str) when is_binary(str) do
    s = str |> String.replace(~r/\s+/u, " ") |> String.trim()

    if String.length(s) <= @def_char_limit do
      s
    else
      String.slice(s, 0, @def_char_limit) <> "…"
    end
  end

  defp gloss(other), do: other |> to_string() |> gloss()

  defp short_id(nil), do: "∅"
  defp short_id(id) when is_binary(id), do: id |> String.split("|") |> hd()

  defp fmt_score(nil), do: "—"
  defp fmt_score(s) when is_integer(s), do: Integer.to_string(s)
  defp fmt_score(s) when is_float(s), do: :erlang.float_to_binary(s, decimals: 2)

  defp fmt_score(s) when is_binary(s) do
    case Float.parse(String.trim(s)) do
      {f, _} -> :erlang.float_to_binary(f, decimals: 2)
      _ -> s
    end
  end

  defp fmt_score(s), do: to_string(s)

  defp norm_text(nil), do: ""

  defp norm_text(v) when is_binary(v),
    do: v |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp norm_text(v),
    do: v |> to_string() |> String.downcase() |> String.replace(~r/\s+/u, " ") |> String.trim()

  defp first_present(list) when is_list(list) do
    list
    |> Enum.find("", fn v -> is_binary(v) and String.trim(v) != "" end)
  end

  defp maybe_put_nonblank(map, _key, nil), do: map

  defp maybe_put_nonblank(map, key, value) when is_binary(value) do
    if String.trim(value) == "", do: map, else: Map.put(map, key, value)
  end

  defp maybe_put_nonblank(map, key, value), do: Map.put(map, key, value)
end
