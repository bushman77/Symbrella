defmodule Core.Semantic.EpisodicEvents do
  @moduledoc """
  Versioned, domain-neutral event frame contract and conservative text adapter.

  Normalized events are entity/relation/value claims with status and attribution.
  This grammar recognizes only a small set of *explicit assertions*; the generic
  event contract is also callable by semantic processors for other relations.
  It never interprets questions, guesses, hypotheticals or LLM replies as facts.
  """

  @max_field 160
  @relation ~r/\A[a-z][a-z0-9_]{0,63}\z/
  @type event :: %{
    entity: String.t(),
    entity_key: String.t(),
    relation: String.t(),
    value: String.t(),
    status: :asserted | :retracted
  }

  @doc "Normalize structured semantic events from any trusted user-input parser."
  @spec normalize(map()) :: {:ok, event()} | :error
  def normalize(event) when is_map(event) do
    entity =
      event
      |> Map.get(:entity, Map.get(event, "entity"))
      |> clean()
      |> String.replace(~r/^my friend\s+/iu, "")
      |> String.replace(~r/^The\s+/u, "the ")
    relation = clean(Map.get(event, :relation) || Map.get(event, "relation")) |> String.downcase()
    value = clean(Map.get(event, :value) || Map.get(event, "value"))
    status = Map.get(event, :status, Map.get(event, "status", :asserted))
    status = if status in [:retracted, "retracted"], do: :retracted, else: status

    cond do
      entity == "" or value == "" or byte_size(value) > @max_field -> :error
      byte_size(entity) > @max_field or not Regex.match?(@relation, relation) -> :error
      status not in [:asserted, :retracted] -> :error
      true ->
        {:ok, %{entity: entity, entity_key: entity_key(entity), relation: relation,
                value: value, status: status}}
    end
  end

  def normalize(_), do: :error

  @doc "Parse zero or more unambiguous statements from one user turn."
  @spec extract(String.t()) :: [event()]
  def extract(text) when is_binary(text) do
    text
    |> String.split(~r/(?<=[.!?])\s+/u, trim: true)
    |> Enum.take(8)
    |> Enum.reject(&String.contains?(&1, "?"))
    |> Enum.map(&statement/1)
    |> Enum.reject(&is_nil/1)
  end

  def extract(_), do: []

  @doc "Map a question onto either a relation slot or the entity's event timeline."
  @spec question(String.t()) ::
          {:relation, String.t(), String.t()} | {:history, String.t()} | nil
  def question(text) when is_binary(text) do
    t = String.trim(text)

    cond do
      m = Regex.named_captures(~r/^what (?:do you |can you )?remember about (?<entity>.+?)\??$/iu, t) ->
        {:history, entity_key(m["entity"])}

      m = Regex.named_captures(~r/^where (?:does|did) (?<entity>.+?) live\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "lives_in"}

      m = Regex.named_captures(~r/^what city does (?<entity>.+?) live in\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "lives_in"}

      m = Regex.named_captures(~r/^where did i meet (?<entity>.+?)\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "met_at"}

      m = Regex.named_captures(~r/^what (?:did we|did i) name (?<entity>.+?)\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "named"}

      m = Regex.named_captures(~r/^when is (?<entity>.+?) (?:scheduled|taking place)\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "scheduled_for"}

      m = Regex.named_captures(~r/^when is (?<entity>.+?)\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "scheduled_for"}

      m = Regex.named_captures(~r/^(?:what|when) is (?<entity>.+?)'?s deadline\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "deadline"}

      m = Regex.named_captures(~r/^(?:what|when) is (?<entity>.+?) deadline\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "deadline"}

      Regex.match?(~r/^what do i have to do\??$/iu, t) ->
        {:relation, "me", "has_obligation"}

      m = Regex.named_captures(~r/^what does (?<entity>.+?) like\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "likes"}

      Regex.match?(~r/^what do i prefer\??$/iu, t) ->
        {:relation, "me", "prefers"}

      m = Regex.named_captures(~r/^where (?:is|was) (?<entity>.+?)\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "located_at"}

      m = Regex.named_captures(~r/^what is (?<entity>.+?)'?s relationship to me\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "relationship"}

      m = Regex.named_captures(~r/^who is (?<entity>.+?) to me\??$/iu, t) ->
        {:relation, entity_key(m["entity"]), "relationship"}

      match = Regex.named_captures(~r/^what (?:did|does) (?<entity>.+?) (?<verb>[a-z]+)\??$/iu, t) ->
        {:relation, entity_key(match["entity"]), "verb_" <> String.downcase(match["verb"])}

      true ->
        nil
    end
  end

  def question(_), do: nil

  @doc "Canonical exact-entity key shared by ingestion and queries."
  @spec entity_key(String.t()) :: String.t()
  def entity_key(s) when is_binary(s) do
    s
    |> clean()
    |> String.downcase()
    |> String.replace(~r/^(?:the|a|an|our|my friend|my)\s+/iu, "")
    |> String.replace(~r/\s+/u, " ")
  end

  defp statement(raw) do
    s = raw |> String.trim() |> String.trim_trailing(".")
    s = Regex.replace(~r/^(?:actually[, ]+|to be clear[, ]+)/iu, s, "")
    # No semantic commitment for hypotheticals, requests, quotes or guesses.
    if Regex.match?(~r/^(?:if|maybe|perhaps|suppose|imagine|i think|i guess|what if|could|would|please|remember that)\b/iu, s) or
         String.contains?(s, ~s(")) do
      nil
    else
      parse_assertion(s)
    end
  end

  defp parse_assertion(s) do
    # All regexes are anchored and capture both an entity and a value.
    rules = [
      {"deadline", ~r/^(?<entity>.+?)'s deadline is (?<value>.+)$/iu},
      {"deadline", ~r/^(?<entity>.+?) deadline is (?<value>.+)$/iu},
      {"lives_in", ~r/^(?<entity>(?:my friend )?[\p{L}\p{N} '-]+?) lives in (?<value>.+)$/iu},
      {"met_at", ~r/^i met (?<entity>.+?) at (?<value>.+)$/iu},
      {"named", ~r/^(?:we|i) named (?<entity>.+?) (?<value>[\p{L}\p{N}-]+)$/iu},
      {"scheduled_for", ~r/^(?<entity>.+?) is scheduled for (?<value>.+)$/iu},
      {"scheduled_for", ~r/^(?<entity>.+?) is on (?<value>(?:monday|tuesday|wednesday|thursday|friday|saturday|sunday)(?: .*)?)$/iu},
      {"likes", ~r/^(?<entity>.+?) likes (?<value>.+)$/iu},
      {"prefers", ~r/^i prefer (?<value>.+)$/iu},
      {"located_at", ~r/^(?<entity>.+?) is located (?<value>(?:at|in|on) .+)$/iu},
      {"relationship", ~r/^(?<entity>.+?) is (?<value>my (?:sister|brother|friend|cousin|colleague|mentor|uncle|aunt))$/iu}
    ]

    Enum.find_value(rules, fn {relation, regex} ->
      case Regex.named_captures(regex, s) do
        %{"value" => value} = captures ->
          entity = Map.get(captures, "entity", "me")
          case normalize(%{entity: entity, relation: relation, value: value}) do
            {:ok, event} -> event
            :error -> nil
          end

        _ -> nil
      end
    end)
  end

  defp clean(value) when is_binary(value) do
    value
    |> String.trim()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim_trailing(".")
    |> String.trim_trailing("?")
    |> String.trim()
  end
  defp clean(_), do: ""
end
