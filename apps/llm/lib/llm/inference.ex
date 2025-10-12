# apps/llm/lib/llm/inference.ex
defmodule Llm.Inference do
  @moduledoc false

  alias Llm.{Http, Util, ModelControl, Daemon}

  # Keep strict behavior local to inference paths.
  @strict_unload_verify_attempts 12
  @strict_unload_verify_sleep_ms 150

  # ─────────────────────────── Public API ───────────────────────────

  @doc """
  Normalize, ensure daemon/model, call /api/chat, handle ephemeral unload.

  Returns:
    {:ok, %{content: String.t(), raw: map()}, new_state}
    {:error, reason, new_state}
  """
  def chat(model \\ nil, prompt, opts, state) do
    with {:ok, msgs} <- normalize_messages(prompt) do
      http_timeout = Keyword.get(opts, :timeout, state.timeout)
      m = model || state.model
      ephemeral? = Keyword.get(opts, :ephemeral?, false)
      opts2 = if ephemeral?, do: Keyword.put(opts, :keep_alive, 0), else: opts

      case ensure_autostart_and_warm(state, m, Keyword.merge(opts2, %{timeout: http_timeout})) do
        {:ok, state2, _warmed?} ->
          base = %{"model" => m, "stream" => false, "messages" => msgs}
          body = Http.build_request_body(base, state2, opts2)

          reply = Http.post_json(state2, "/api/chat", body, Keyword.put(opts2, :timeout, http_timeout))
          state3 = maybe_ephemeral_cleanup_if(state2, m, ephemeral?, http_timeout)

          case reply do
            {:ok, %{"message" => %{"content" => content}} = raw} ->
              {:ok, %{content: content, raw: raw}, state3}

            {:ok, raw} ->
              {:error, {:unexpected_response, raw}, state3}

            {:error, reason} ->
              {:error, reason, state3}
          end

        {:error, reason, state2} ->
          state3 = maybe_ephemeral_cleanup_if(state2, m, ephemeral?, http_timeout)
          {:error, reason, state3}
      end
    else
      {:error, reason} -> {:error, reason, state}
    end
  end

  @doc """
  Ensure daemon/model, call /api/generate, handle ephemeral unload.

  Returns:
    {:ok, %{response: String.t(), raw: map()}, new_state}
    {:error, reason, new_state}
  """
  def generate(model \\ nil, prompt, opts, state) when is_binary(prompt) do
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    m = model || state.model
    ephemeral? = Keyword.get(opts, :ephemeral?, false)
    opts2 = if ephemeral?, do: Keyword.put(opts, :keep_alive, 0), else: opts

    case ensure_autostart_and_warm(state, m, Keyword.merge(opts2, %{timeout: http_timeout})) do
      {:ok, state2, _warmed?} ->
        base = %{"model" => m, "prompt" => prompt, "stream" => false}
        body = Http.build_request_body(base, state2, opts2)

        reply = Http.post_json(state2, "/api/generate", body, Keyword.put(opts2, :timeout, http_timeout))
        state3 = maybe_ephemeral_cleanup_if(state2, m, ephemeral?, http_timeout)

        case reply do
          {:ok, %{"response" => resp} = raw} ->
            {:ok, %{response: resp, raw: raw}, state3}

          {:ok, raw} ->
            {:error, {:unexpected_response, raw}, state3}

          {:error, reason} ->
            {:error, reason, state3}
        end

      {:error, reason, state2} ->
        state3 = maybe_ephemeral_cleanup_if(state2, m, ephemeral?, http_timeout)
        {:error, reason, state3}
    end
  end

  @doc """
  Single text embeddings via /api/embeddings, with strict ephemeral support.

  Returns:
    {:ok, %{embeddings: list(), raw: map()}, new_state}
    {:error, reason, new_state}
  """
  def embeddings_one(text, opts, state) when is_binary(text) do
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    model = Keyword.get(opts, :model, "nomic-embed-text")
    ephemeral? = Keyword.get(opts, :ephemeral?, false)
    opts2 = if ephemeral?, do: Keyword.put(opts, :keep_alive, 0), else: opts

    case ensure_autostart_and_warm(state, model, Keyword.merge(opts2, %{timeout: http_timeout})) do
      {:ok, state2, _warmed?} ->
        body = %{"model" => model, "prompt" => text} |> Util.maybe_put_keep_alive(opts2)

        reply = Http.post_json(state2, "/api/embeddings", body, Keyword.put(opts2, :timeout, http_timeout))
        state3 = maybe_ephemeral_cleanup_if(state2, model, ephemeral?, http_timeout)

        case reply do
          {:ok, %{"embedding" => vec} = raw} when is_list(vec) ->
            {:ok, %{embeddings: vec, raw: raw}, state3}

          {:ok, raw} ->
            {:error, {:unexpected_response, raw}, state3}

          {:error, reason} ->
            {:error, reason, state3}
        end

      {:error, reason, state2} ->
        state3 = maybe_ephemeral_cleanup_if(state2, model, ephemeral?, http_timeout)
        {:error, reason, state3}
    end
  end

  @doc """
  Batch embeddings; strict on failures with {:at, idx, reason}.

  Returns:
    {:ok, [%{embeddings: list(), raw: map()}], new_state}
    {:error, reason, new_state}
  """
  def embeddings_batch(list, opts, state) when is_list(list) do
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    model = Keyword.get(opts, :model, "nomic-embed-text")
    ephemeral? = Keyword.get(opts, :ephemeral?, false)
    opts2 = if ephemeral?, do: Keyword.put(opts, :keep_alive, 0), else: opts

    case ensure_autostart_and_warm(state, model, Keyword.merge(opts2, %{timeout: http_timeout})) do
      {:ok, state2, _warmed?} ->
        result =
          Enum.reduce_while(Enum.with_index(list), {:ok, []}, fn
            {text, idx}, {:ok, acc} when is_binary(text) ->
              body = %{"model" => model, "prompt" => text} |> Util.maybe_put_keep_alive(opts2)

              case Http.post_json(state2, "/api/embeddings", body, Keyword.put(opts2, :timeout, http_timeout)) do
                {:ok, %{"embedding" => vec} = raw} when is_list(vec) ->
                  {:cont, {:ok, acc ++ [%{embeddings: vec, raw: raw}]}}
                {:ok, raw} ->
                  {:halt, {:error, {:at, idx, {:unexpected_response, raw}}}}
                {:error, reason} ->
                  {:halt, {:error, {:at, idx, reason}}}
              end

            {bad, idx}, _ ->
              {:halt, {:error, {:bad_item, idx, bad}}}
          end)

        state3 = maybe_ephemeral_cleanup_if(state2, model, ephemeral?, http_timeout)
        case result do
          {:ok, out}      -> {:ok, out, state3}
          {:error, reason} -> {:error, reason, state3}
        end

      {:error, reason, state2} ->
        state3 = maybe_ephemeral_cleanup_if(state2, model, ephemeral?, http_timeout)
        {:error, reason, state3}
    end
  end

  @doc """
  Public helper: ensure daemon and optionally pull/warm a model.

  Returns:
    {:ok, new_state, warmed?} | {:error, reason, new_state}
  """
  def ensure_autostart_and_warm(state, model, opts) do
    auto? = Keyword.get(opts, :auto_start?, true)
    pull? = Keyword.get(opts, :pull?, true)
    warm? = Keyword.get(opts, :warm?, true) and not Keyword.get(opts, :ephemeral?, false)
    http_timeout = Keyword.get(opts, :timeout, state.timeout)
    m = model || state.model

    if auto? do
      case Daemon.ensure_serving(state, http_timeout) do
        {:ok, st2, _spawned?} ->
          _ = if pull?, do: ModelControl.pull_model(m), else: true

          if warm? do
            case ModelControl.maybe_warm_model(st2, m, http_timeout) do
              {:ok, st3}   -> {:ok, st3, true}
              {:error, _}  -> {:ok, st2, false}
            end
          else
            {:ok, st2, false}
          end

        {:error, st2, reason} ->
          {:error, reason, st2}
      end
    else
      {:ok, state, false}
    end
  end

  # ─────────────────────────── Private ──────────────────────────────

  # Normalize chat prompt into Ollama messages
  defp normalize_messages(text) when is_binary(text) do
    {:ok, [%{"role" => "user", "content" => text}]}
  end
  defp normalize_messages([%{} | _] = msgs) do
    {:ok,
     Enum.map(msgs, fn
       %{"role" => r, "content" => c} -> %{"role" => r, "content" => c}
       %{role: r, content: c} -> %{"role" => r, "content" => c}
       other -> other
     end)}
  end
  defp normalize_messages(other), do: {:error, {:bad_messages, other}}

  # For ephemeral calls: strict unload + drop warmed flag.
  defp maybe_ephemeral_cleanup_if(state, model, true, http_timeout) do
    _ = ModelControl.unload_model_strict(state, model,
          timeout: http_timeout,
          attempts: @strict_unload_verify_attempts,
          sleep_ms: @strict_unload_verify_sleep_ms)

    %{state | warmed_models: MapSet.delete(state.warmed_models, model)}
  end
  defp maybe_ephemeral_cleanup_if(state, _model, false, _http_timeout), do: state
end

