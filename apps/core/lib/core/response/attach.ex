defmodule Core.Response.Attach do
  @moduledoc """
  Attaches response planning output onto an SI (SemanticInput or map).

  Writes:
    • :response_tone
    • :response_text
    • :response_meta

  Design goals:
    • Never crash the pipeline (best-effort).
    • Accept %SemanticInput{} or a plain map.
    • Pull intent/keyword/confidence from (in order):
        1) SI fields
        2) si.emotion.from (Amygdala output)
        3) latest {:intent, %{...}} in si.trace
        4) fallback (:unknown, 0.0, sentence-as-text)
    • Optionally apply response guardrails if present.
  """

  alias Core.SemanticInput

  @type opts :: keyword()

  @stopwords MapSet.new([
               "a", "an", "the", "and", "or", "to", "of", "in", "on", "for", "with", "at", "by",
               "is", "are", "was", "were", "be", "been", "being",
               "i", "you", "he", "she", "it", "we", "they", "me", "him", "her", "us", "them",
               "my", "your", "his", "her", "its", "our", "their",
               "this", "that", "these", "those"
             ])

  @spec maybe_build_response_plan(SemanticInput.t() | map(), opts()) :: SemanticInput.t() | map()
  def maybe_build_response_plan(%{} = si, opts) do
    case Keyword.get(opts, :response, :auto) do
      :off -> si
      false -> si
      _ -> build_and_attach(si, opts)
    end
  end

  def maybe_build_response_plan(other, _opts), do: other

  defp build_and_attach(%{} = si, opts) do
    if Code.ensure_loaded?(Core.Response) and function_exported?(Core.Response, :plan, 2) do
      si_like = build_response_si_like(si)
      mood_like = build_response_mood_like(si, opts)

      case Core.Response.plan(si_like, mood_like) do
        {tone, text, meta} ->
          meta2 =
            meta
            |> ensure_map()
            |> Map.put_new(:allowed_norms, allowed_norms_from_tokens(si_get(si, :tokens)))

          {tone2, text2, meta3} = maybe_apply_guardrails(si, {tone, text, meta2}, opts)

          si
          |> si_put(:response_tone, tone2)
          |> si_put(:response_text, text2)
          |> si_put(:response_meta, meta3)

        _ ->
          si
      end
    else
      si
    end
  rescue
    _ -> si
  catch
    _, _ -> si
  end

  # ─────────────────────── input shaping ───────────────────────

  defp build_response_si_like(%{} = si) do
    {intent, confidence, keyword, text} = pick_intent_conf_keyword_and_text(si)

    %{
      intent: intent,
      keyword: keyword,
      confidence: confidence,
      text: text
    }
  end

  defp pick_intent_conf_keyword_and_text(%{} = si) do
    from_emotion = get_in_map(si, [:emotion, :from]) || %{}
    from_trace = find_intent_trace(si) || %{}

    base_text =
      si_get(si, :sentence) ||
        si_get(si, :text) ||
        si_get(si, :keyword) ||
        Map.get(from_emotion, :keyword) ||
        Map.get(from_trace, :keyword) ||
        ""

    intent_si = si_get(si, :intent)
    intent_em = Map.get(from_emotion, :intent)
    intent_tr = Map.get(from_trace, :intent)

    intent =
      cond do
        intent_si not in [nil, :unknown, :other] -> intent_si
        intent_em not in [nil, :unknown, :other] -> intent_em
        intent_tr not in [nil, :unknown, :other] -> intent_tr
        true -> :unknown
      end

    conf_si = si_get(si, :confidence)
    conf_em = Map.get(from_emotion, :confidence)
    conf_tr = Map.get(from_trace, :confidence)

    confidence =
      cond do
        is_number(conf_si) -> conf_si
        is_number(conf_em) -> conf_em
        is_number(conf_tr) -> conf_tr
        true -> 0.0
      end

    keyword =
      si_get(si, :keyword) ||
        Map.get(from_emotion, :keyword) ||
        Map.get(from_trace, :keyword) ||
        base_text

    {intent, confidence, keyword, to_string(base_text)}
  end

  defp find_intent_trace(%{trace: tr}) when is_list(tr) do
    tr
    |> Enum.reverse()
    |> Enum.find_value(fn
      {:intent, %{} = payload} -> payload
      %{stage: :intent, meta: %{} = meta} -> meta
      _ -> nil
    end)
  end

  defp find_intent_trace(_), do: nil

  defp build_response_mood_like(%{} = si, _opts) do
    case si_get(si, :mood) do
      %{} = mood ->
        %{mood: mood, tone_hint: tone_hint_from_emotion(si)}

      _ ->
        emotion = si_get(si, :emotion) || %{}
        latents = Map.get(emotion, :latents, %{})

        control = clamp01(Map.get(latents, :control, 0.0))
        threat = clamp01(Map.get(latents, :threat, 0.0))
        safety = clamp01(Map.get(latents, :safety, 0.0))
        reward = clamp01(Map.get(latents, :reward, 0.0))

        vigilance =
          clamp01(
            0.6 * threat +
              0.2 * (1.0 - safety) +
              0.2 * control
          )

        exploration = reward
        inhibition = control

        plasticity =
          clamp01(
            0.5 +
              0.3 * (reward - threat)
          )

        %{
          mood: %{
            exploration: exploration,
            inhibition: inhibition,
            vigilance: vigilance,
            plasticity: plasticity
          },
          tone_hint: tone_hint_from_emotion(si)
        }
    end
  end

  defp tone_hint_from_emotion(%{} = si) do
    case si_get(si, :emotion) do
      %{} = em -> Map.get(em, :tone_reaction)
      _ -> nil
    end
  end

  # ─────────────────────── guardrails (optional) ───────────────────────

  defp maybe_apply_guardrails(_si, {tone, text, meta}, _opts) do
    mod = Core.Response.Guardrails

    cond do
      Code.ensure_loaded?(mod) and function_exported?(mod, :run, 3) ->
        ctx = %{allowed_norms: Map.get(meta, :allowed_norms)}
        normalize_guardrails_return(mod.run(text, ctx, meta), tone, text, meta)

      Code.ensure_loaded?(mod) and function_exported?(mod, :enforce, 3) ->
        ctx = %{allowed_norms: Map.get(meta, :allowed_norms)}
        normalize_guardrails_return(mod.enforce(text, ctx, meta), tone, text, meta)

      Code.ensure_loaded?(mod) and function_exported?(mod, :apply, 3) ->
        ctx = %{allowed_norms: Map.get(meta, :allowed_norms)}
        normalize_guardrails_return(mod.apply(text, ctx, meta), tone, text, meta)

      true ->
        {tone, text, meta}
    end
  rescue
    _ -> {tone, text, meta}
  end

  defp normalize_guardrails_return(ret, tone, text, meta) do
    case ret do
      {t2, txt2, meta2} -> {t2, txt2, ensure_map(meta2)}
      {txt2, meta2} -> {tone, txt2, ensure_map(meta2)}
      txt2 when is_binary(txt2) -> {tone, txt2, meta}
      _ -> {tone, text, meta}
    end
  end

  # ─────────────────────── allowlist helper ───────────────────────

  defp allowed_norms_from_tokens(tokens) when is_list(tokens) do
    tokens
    |> Enum.flat_map(fn
      %{phrase: p} = tok ->
        n = Map.get(tok, :n, 1)
        mw = Map.get(tok, :mw, false)

        if n == 1 and mw in [false, nil] do
          [norm_text(p)]
        else
          []
        end

      p when is_binary(p) ->
        [norm_text(p)]

      _ ->
        []
    end)
    |> Enum.reject(&(&1 in [nil, ""]))
    |> Enum.reject(&MapSet.member?(@stopwords, &1))
    |> Enum.reject(&(Regex.match?(~r/\s/u, &1)))
    |> MapSet.new()
  end

  defp allowed_norms_from_tokens(_), do: MapSet.new()

  # ─────────────────────── small utils ───────────────────────

  defp norm_text(v) do
    cond do
      Code.ensure_loaded?(Core.Text) and function_exported?(Core.Text, :normalize, 1) ->
        Core.Text.normalize(v)

      true ->
        v
        |> to_string()
        |> String.downcase()
        |> String.trim()
        |> String.replace(~r/^\p{P}+/u, "")
        |> String.replace(~r/\p{P}+$/u, "")
        |> String.replace(~r/\s+/u, " ")
    end
  end

  defp clamp01(x) when is_number(x), do: min(1.0, max(0.0, x))
  defp clamp01(_), do: 0.0

  defp ensure_map(%{} = m), do: m
  defp ensure_map(_), do: %{}

  defp si_get(%SemanticInput{} = si, k), do: Map.get(si, k)
  defp si_get(%{} = si, k), do: Map.get(si, k)
  defp si_get(_, _), do: nil

  defp si_put(%SemanticInput{} = si, k, v), do: Map.put(si, k, v)
  defp si_put(%{} = si, k, v), do: Map.put(si, k, v)
  defp si_put(si, _k, _v), do: si

  defp get_in_map(map, path) when is_map(map) do
    Enum.reduce(path, map, fn key, acc ->
      case acc do
        %{} = m -> Map.get(m, key)
        _ -> nil
      end
    end)
  end
end

