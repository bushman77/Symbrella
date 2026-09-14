# apps/llm/lib/llm/inference.ex
defmodule Llm.Inference do
  @moduledoc false

  alias Llm.{Daemon, Http, ModelControl, Util}

  # Keep strict behavior local to inference paths.
  @strict_unload_verify_attempts 12
  @strict_unload_verify_sleep_ms 150

  # Chat generation must end with the assistant's current turn.
  #
  # These are a first-line defense. The returned text is still sanitized
  # afterward because model stop handling should never be treated as a
  # correctness boundary.
  @chat_stop_sequences [
    "\nuser\n",
    "\nuser:",
    "\nUser\n",
    "\nUser:",
    "\nUSER\n",
    "\nUSER:",
    "\nsystem\n",
    "\nsystem:",
    "\nSystem\n",
    "\nSystem:",
    "<|im_start|>user",
    "<|im_start|>system",
    "<|im_end|>",
    "<|eot_id|>",
    "<|endoftext|>",
    "<|end_of_text|>"
  ]

  # ─────────────────────────── Public API ───────────────────────────

  @doc """
  Normalize, ensure daemon/model (cold-by-default), call /api/chat,
  sanitize the assistant turn, and handle ephemeral unload.

  Returns:

      {:ok, %{content: String.t(), raw: map()}, new_state}
      {:error, reason, new_state}
  """
  def chat(model \\ nil, prompt, opts, state) do
    with {:ok, messages} <- normalize_messages(prompt) do
      http_timeout = Keyword.get(opts, :timeout, state.timeout)
      model = model || state.model
      ephemeral? = Keyword.get(opts, :ephemeral?, false)

      opts =
        opts
        |> normalize_ephemeral_opts(ephemeral?)
        |> ensure_chat_stop_sequences()

      case ensure_autostart_and_warm(
             state,
             model,
             Keyword.merge(opts,
               timeout: http_timeout,
               origin: :chat
             )
           ) do
        {:ok, state2, _warmed?} ->
          body =
            %{
              "model" => model,
              "stream" => false,
              "messages" => messages
            }
            |> Http.build_request_body(state2, opts)

          reply =
            Http.post_json(
              state2,
              "/api/chat",
              body,
              Keyword.put(opts, :timeout, http_timeout)
            )

          state3 =
            maybe_ephemeral_cleanup_if(
              state2,
              model,
              ephemeral?,
              http_timeout
            )

          handle_chat_reply(reply, state3)

        {:error, reason, state2} ->
          state3 =
            maybe_ephemeral_cleanup_if(
              state2,
              model,
              ephemeral?,
              http_timeout
            )

          {:error, reason, state3}
      end
    else
      {:error, reason} ->
        {:error, reason, state}
    end
  end

  @doc """
  Ensure daemon/model (cold-by-default), call /api/generate,
  and handle ephemeral unload.

  Returns:

      {:ok, %{response: String.t(), raw: map()}, new_state}
      {:error, reason, new_state}
  """
  def generate(model \\ nil, prompt, opts, state) when is_binary(prompt) do
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    model = model || state.model
    ephemeral? = Keyword.get(opts, :ephemeral?, false)

    opts = normalize_ephemeral_opts(opts, ephemeral?)

    case ensure_autostart_and_warm(
           state,
           model,
           Keyword.merge(opts,
             timeout: http_timeout,
             origin: :generate
           )
         ) do
      {:ok, state2, _warmed?} ->
        body =
          %{
            "model" => model,
            "prompt" => prompt,
            "stream" => false
          }
          |> Http.build_request_body(state2, opts)

        reply =
          Http.post_json(
            state2,
            "/api/generate",
            body,
            Keyword.put(opts, :timeout, http_timeout)
          )

        state3 =
          maybe_ephemeral_cleanup_if(
            state2,
            model,
            ephemeral?,
            http_timeout
          )

        case reply do
          {:ok, %{"response" => response} = raw} ->
            {:ok, %{response: response, raw: raw}, state3}

          {:ok, raw} ->
            {:error, {:unexpected_response, raw}, state3}

          {:error, reason} ->
            {:error, reason, state3}
        end

      {:error, reason, state2} ->
        state3 =
          maybe_ephemeral_cleanup_if(
            state2,
            model,
            ephemeral?,
            http_timeout
          )

        {:error, reason, state3}
    end
  end

  @doc """
  Single text embeddings via /api/embeddings, with strict ephemeral support
  (cold-by-default).

  Returns:

      {:ok, %{embeddings: list(), raw: map()}, new_state}
      {:error, reason, new_state}
  """
  def embeddings_one(text, opts, state) when is_binary(text) do
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    model = Keyword.get(opts, :model, "nomic-embed-text")
    ephemeral? = Keyword.get(opts, :ephemeral?, false)

    opts = normalize_ephemeral_opts(opts, ephemeral?)

    case ensure_autostart_and_warm(
           state,
           model,
           Keyword.merge(opts,
             timeout: http_timeout,
             origin: :embeddings_one
           )
         ) do
      {:ok, state2, _warmed?} ->
        body =
          %{
            "model" => model,
            "prompt" => text
          }
          |> Util.maybe_put_keep_alive(opts)

        reply =
          Http.post_json(
            state2,
            "/api/embeddings",
            body,
            Keyword.put(opts, :timeout, http_timeout)
          )

        state3 =
          maybe_ephemeral_cleanup_if(
            state2,
            model,
            ephemeral?,
            http_timeout
          )

        case reply do
          {:ok, %{"embedding" => vector} = raw} when is_list(vector) ->
            {:ok, %{embeddings: vector, raw: raw}, state3}

          {:ok, raw} ->
            {:error, {:unexpected_response, raw}, state3}

          {:error, reason} ->
            {:error, reason, state3}
        end

      {:error, reason, state2} ->
        state3 =
          maybe_ephemeral_cleanup_if(
            state2,
            model,
            ephemeral?,
            http_timeout
          )

        {:error, reason, state3}
    end
  end

  @doc """
  Batch embeddings; strict on failures with `{:at, idx, reason}`
  (cold-by-default).

  Returns:

      {:ok, [%{embeddings: list(), raw: map()}], new_state}
      {:error, reason, new_state}
  """
  def embeddings_batch(list, opts, state) when is_list(list) do
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    model = Keyword.get(opts, :model, "nomic-embed-text")
    ephemeral? = Keyword.get(opts, :ephemeral?, false)

    opts = normalize_ephemeral_opts(opts, ephemeral?)

    case ensure_autostart_and_warm(
           state,
           model,
           Keyword.merge(opts,
             timeout: http_timeout,
             origin: :embeddings_batch
           )
         ) do
      {:ok, state2, _warmed?} ->
        result =
          Enum.reduce_while(
            Enum.with_index(list),
            {:ok, []},
            fn
              {text, idx}, {:ok, acc} when is_binary(text) ->
                body =
                  %{
                    "model" => model,
                    "prompt" => text
                  }
                  |> Util.maybe_put_keep_alive(opts)

                case Http.post_json(
                       state2,
                       "/api/embeddings",
                       body,
                       Keyword.put(opts, :timeout, http_timeout)
                     ) do
                  {:ok, %{"embedding" => vector} = raw}
                  when is_list(vector) ->
                    {:cont,
                     {:ok,
                      acc ++
                        [
                          %{
                            embeddings: vector,
                            raw: raw
                          }
                        ]}}

                  {:ok, raw} ->
                    {:halt, {:error, {:at, idx, {:unexpected_response, raw}}}}

                  {:error, reason} ->
                    {:halt, {:error, {:at, idx, reason}}}
                end

              {bad, idx}, _acc ->
                {:halt, {:error, {:bad_item, idx, bad}}}
            end
          )

        state3 =
          maybe_ephemeral_cleanup_if(
            state2,
            model,
            ephemeral?,
            http_timeout
          )

        case result do
          {:ok, out} ->
            {:ok, out, state3}

          {:error, reason} ->
            {:error, reason, state3}
        end

      {:error, reason, state2} ->
        state3 =
          maybe_ephemeral_cleanup_if(
            state2,
            model,
            ephemeral?,
            http_timeout
          )

        {:error, reason, state3}
    end
  end

  @doc """
  Public helper: ensure daemon and optionally pull/warm a model.

  Cold-by-default:

    * `auto_start?` — default: false
    * `pull?`       — default: false
    * `warm?`       — default: false, and never when `ephemeral?` is true

  Manual-only global guard:

  Set:

      config :llm, manual_only?: true

  to block autostart unless the caller passes `force?: true`.

  Returns:

      {:ok, new_state, warmed?}
      {:error, reason, new_state}
  """
  def ensure_autostart_and_warm(state, model, opts) do
    manual_only? =
      Application.get_env(
        :llm,
        :manual_only?,
        false
      )

    force? = Keyword.get(opts, :force?, false)

    auto_start? =
      Keyword.get(
        opts,
        :auto_start?,
        false
      )

    pull? =
      Keyword.get(
        opts,
        :pull?,
        false
      )

    warm? =
      Keyword.get(
        opts,
        :warm?,
        false
      ) and
        not Keyword.get(
          opts,
          :ephemeral?,
          false
        )

    http_timeout =
      Keyword.get(
        opts,
        :timeout,
        state.timeout
      )

    origin =
      Keyword.get(
        opts,
        :origin,
        :unknown
      )

    model = model || state.model

    cond do
      manual_only? and not force? ->
        {:error, :manual_only, state}

      auto_start? or force? ->
        ensure_daemon_and_model(
          state,
          model,
          pull?,
          warm?,
          http_timeout,
          origin
        )

      true ->
        # No autostart; return state unchanged.
        {:ok, state, false}
    end
  end

  # ─────────────────────── Chat response safety ─────────────────────

  defp handle_chat_reply(
         {:ok, %{"message" => %{"content" => content}} = raw},
         state
       )
       when is_binary(content) do
    sanitized = sanitize_model_text(content)

    {:ok,
     %{
       content: sanitized,
       raw: raw
     }, state}
  end

  defp handle_chat_reply({:ok, raw}, state) do
    {:error, {:unexpected_response, raw}, state}
  end

  defp handle_chat_reply({:error, reason}, state) do
    {:error, reason, state}
  end

  # A chat model is allowed to return only its current assistant turn.
  #
  # The sanitizer deliberately acts as a second defensive boundary even
  # though stop sequences are supplied in the request. A model may ignore,
  # partially honor, or tokenize stop sequences unexpectedly.
  defp sanitize_model_text(text) when is_binary(text) do
    text
    |> normalize_model_control_tokens()
    |> strip_leading_assistant_marker()
    |> truncate_generated_role_continuation()
    |> String.trim()
  end

  # Convert chat-template control markers into boundaries instead of simply
  # concatenating the surrounding text.
  #
  # Example:
  #
  #   "...answer<|im_start|>user\nfake continuation"
  #
  # becomes:
  #
  #   "...answer\nuser\nfake continuation"
  #
  # which allows the role-boundary sanitizer below to remove it safely.
  defp normalize_model_control_tokens(text) do
    text
    |> String.replace(
      ~r/<\|im_start\|>/u,
      "\n"
    )
    |> String.replace(
      ~r/<\|(?:im_end|eot_id|endoftext|end_of_text)\|>/u,
      "\n"
    )
  end

  # Some models may expose the assistant role marker itself.
  #
  # Strip it only when it appears at the very beginning of the generated
  # content. Later assistant/user/system markers are treated as attempted
  # continuation boundaries.
  defp strip_leading_assistant_marker(text) do
    text
    |> String.replace(
      ~r/\A[ \t]*assistant[ \t]*:[ \t]*/iu,
      ""
    )
    |> String.replace(
      ~r/\A[ \t]*assistant[ \t]*(?:\r?\n)+/iu,
      ""
    )
  end

  # Keep only the first generated assistant turn.
  #
  # We intentionally recognize role headers only when they occupy a line
  # by themselves or use the conventional "Role:" form. This avoids
  # truncating ordinary prose containing words such as "user experience".
  defp truncate_generated_role_continuation(text) do
    lines =
      String.split(
        text,
        ~r/\r?\n/u,
        trim: false
      )

    {kept, _seen_content?} =
      Enum.reduce_while(
        lines,
        {[], false},
        fn line, {acc, seen_content?} ->
          trimmed = String.trim(line)

          cond do
            seen_content? and generated_role_header?(trimmed) ->
              {:halt, {acc, seen_content?}}

            true ->
              seen_content? =
                seen_content? or
                  trimmed != ""

              {:cont,
               {
                 [line | acc],
                 seen_content?
               }}
          end
        end
      )

    kept
    |> Enum.reverse()
    |> Enum.join("\n")
  end

  defp generated_role_header?(""), do: false

  defp generated_role_header?(line) when is_binary(line) do
    Regex.match?(
      ~r/^(?:user|assistant|system)\s*$/iu,
      line
    ) or
      Regex.match?(
        ~r/^(?:user|assistant|system)\s*:/iu,
        line
      )
  end

  # ─────────────────────── Request preparation ──────────────────────

  defp normalize_ephemeral_opts(opts, true) do
    Keyword.put(opts, :keep_alive, 0)
  end

  defp normalize_ephemeral_opts(opts, false), do: opts

  # Preserve caller-provided stop sequences and append Symbrella's chat
  # turn-boundary protections.
  defp ensure_chat_stop_sequences(opts) do
    options =
      opts
      |> Keyword.get(:options, %{})
      |> normalize_options_map()

    existing_stops =
      options
      |> option_value(:stop, [])
      |> normalize_stop_sequences()

    stops =
      existing_stops
      |> Kernel.++(@chat_stop_sequences)
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()

    options =
      options
      |> Map.delete("stop")
      |> Map.delete(:stop)
      |> Map.put(:stop, stops)

    Keyword.put(
      opts,
      :options,
      options
    )
  end

  defp normalize_options_map(%{} = options), do: options

  defp normalize_options_map(options) when is_list(options) do
    Map.new(options)
  end

  defp normalize_options_map(_), do: %{}

  defp option_value(options, key, default)
       when is_map(options) and is_atom(key) do
    Map.get(
      options,
      key,
      Map.get(
        options,
        Atom.to_string(key),
        default
      )
    )
  end

  defp normalize_stop_sequences(nil), do: []

  defp normalize_stop_sequences(stop) when is_binary(stop) do
    [stop]
  end

  defp normalize_stop_sequences(stops) when is_list(stops) do
    stops
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp normalize_stop_sequences(_), do: []

  # ─────────────────────── Daemon/model control ─────────────────────

  defp ensure_daemon_and_model(
         state,
         model,
         pull?,
         warm?,
         http_timeout,
         origin
       ) do
    case Daemon.ensure_serving(
           state,
           http_timeout
         ) do
      {:ok, state2, spawned?} ->
        safe_telemetry(
          [:llm, :autostart],
          %{count: 1},
          %{
            origin: origin,
            model: model,
            spawned?: spawned?
          }
        )

        if pull? do
          _ = ModelControl.pull_model(model)
        end

        maybe_warm_model(
          state2,
          model,
          warm?,
          http_timeout,
          origin
        )

      {:error, state2, reason} ->
        {:error, reason, state2}
    end
  end

  defp maybe_warm_model(
         state,
         _model,
         false,
         _http_timeout,
         _origin
       ) do
    {:ok, state, false}
  end

  defp maybe_warm_model(
         state,
         model,
         true,
         http_timeout,
         origin
       ) do
    case ModelControl.maybe_warm_model(
           state,
           model,
           http_timeout
         ) do
      {:ok, state2} ->
        safe_telemetry(
          [:llm, :warm],
          %{count: 1},
          %{
            origin: origin,
            model: model
          }
        )

        {:ok, state2, true}

      {:error, _reason} ->
        {:ok, state, false}
    end
  end

  # ───────────────────── Message normalization ──────────────────────

  defp normalize_messages(text) when is_binary(text) do
    {:ok,
     [
       %{
         "role" => "user",
         "content" => text
       }
     ]}
  end

  defp normalize_messages([%{} | _] = messages) do
    normalized =
      Enum.map(
        messages,
        fn
          %{
            "role" => role,
            "content" => content
          } ->
            %{
              "role" => role,
              "content" => content
            }

          %{
            role: role,
            content: content
          } ->
            %{
              "role" => role,
              "content" => content
            }

          other ->
            other
        end
      )

    {:ok, normalized}
  end

  defp normalize_messages(other) do
    {:error, {:bad_messages, other}}
  end

  # ─────────────────────── Ephemeral cleanup ────────────────────────

  defp maybe_ephemeral_cleanup_if(
         state,
         model,
         true,
         http_timeout
       ) do
    _ =
      ModelControl.unload_model_strict(
        state,
        model,
        timeout: http_timeout,
        attempts: @strict_unload_verify_attempts,
        sleep_ms: @strict_unload_verify_sleep_ms
      )

    %{
      state
      | warmed_models:
          MapSet.delete(
            state.warmed_models,
            model
          )
    }
  end

  defp maybe_ephemeral_cleanup_if(
         state,
         _model,
         false,
         _http_timeout
       ) do
    state
  end

  # ─────────────────────────── Telemetry ────────────────────────────

  # Avoid crashing if :telemetry isn't present for any reason.
  defp safe_telemetry(
         event,
         measurements,
         metadata
       ) do
    try do
      :telemetry.execute(
        event,
        measurements,
        metadata
      )
    rescue
      _ ->
        :ok
    end
  end
end
