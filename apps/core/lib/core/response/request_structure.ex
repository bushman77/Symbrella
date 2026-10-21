defmodule Core.Response.RequestStructure do
  @moduledoc """
  Read-only bridge from Core's current-turn LIFG grammatical proposals to
  response expression. It rechecks the original token surfaces, argument
  indices, role evidence and module-name attachment before exposing a small
  advisory summary. It never invents word senses or verifies code semantics.

  This structure is untrusted *user text* for response generation, not a
  developer instruction, fact, executable command, or persistence receipt.
  """

  @max_tokens 64
  @max_surface_bytes 96

  @spec from_si(map()) :: map() | nil
  def from_si(%{} = si) do
    sentence = Map.get(si, :sentence)
    roles = Map.get(si, :role_competition)
    tokens = Map.get(si, :tokens)

    with true <- is_binary(sentence) and byte_size(sentence) <= 8192,
         %{roles: supported, argument_frames: arguments,
           identifier_bindings: bindings, unresolved_tokens: unknown} <- roles,
         true <- is_map(supported) and is_list(arguments) and
                   is_list(bindings) and is_list(unknown),
         true <- is_list(tokens) and length(tokens) <= @max_tokens,
         {:ok, surfaces} <- token_surfaces(tokens, sentence),
         [argument] <- arguments,
         {:ok, frame} <- validated_argument(argument, supported, unknown, surfaces) do
      result = %{
        version: 1,
        status: :grammatical_candidate,
        source: :current_turn_original_tokens,
        predicate: frame.predicate,
        recipient: frame.recipient,
        requested_object: frame.requested_object,
        object_modifier: frame.object_modifier,
        object_lexically_resolved?: frame.object_lexically_resolved?,
        meaning_verified?: false,
        code_semantics_verified?: false
      }

      case validated_binding(bindings, argument, surfaces, unknown) do
        {:ok, binding} -> Map.put(result, :module_name, binding)
        :none -> result
      end
    else
      _ -> nil
    end
  end

  def from_si(_), do: nil

  @doc """
  Short prompt annotation. This is explicitly uncertain grammatical structure;
  the original user message remains authoritative for requested behavior.
  """
  @spec prompt(map() | nil) :: String.t()
  def prompt(%{version: 1, status: :grammatical_candidate,
               source: :current_turn_original_tokens,
               predicate: verb, requested_object: object,
               recipient: recipient, object_modifier: modifier} = frame)
      when is_binary(verb) and is_binary(object) do
    parts = [
      "predicate=#{inspect(verb)}",
      "recipient=#{inspect(recipient)}",
      "requested_object=#{inspect(object)}",
      "object_modifier=#{inspect(modifier)}",
      "module_name=#{inspect(Map.get(frame, :module_name))}"
    ]

    "CURRENT-TURN REQUEST STRUCTURE (provisional grammar; original user text " <>
      "takes precedence): " <> Enum.join(parts, "; ") <> ". " <>
      "Unknown technical identifiers and exact dictionary senses remain unresolved; " <>
      "do not mistake missing lexicon entries for an invalid request. " <>
      "When the user's actual request asks for code, provide useful code if able " <>
      "and permitted, retaining original identifier capitalization. " <>
      "Never claim the requested module already exists or that code was executed."
  end

  def prompt(_), do: ""

  defp token_surfaces(tokens, sentence) do
    nodes =
      tokens
      |> Enum.filter(fn t -> is_map(t) and Map.get(t, :n, 1) == 1 end)

    indexed =
      Enum.reduce_while(nodes, %{}, fn token, acc ->
        index = Map.get(token, :index)
        phrase = Map.get(token, :phrase)
        span = Map.get(token, :span)

        valid? =
          is_integer(index) and index >= 0 and index < @max_tokens and
            is_binary(phrase) and byte_size(phrase) in 1..@max_surface_bytes and
            match?({start, stop} when is_integer(start) and is_integer(stop), span) and
            case span do
              {start, stop} when start >= 0 and stop > start and stop <= byte_size(sentence) ->
                binary_part(sentence, start, stop - start) == phrase
              _ -> false
            end

        if valid? and not Map.has_key?(acc, index) do
          {:cont, Map.put(acc, index, strip_terminal(phrase))}
        else
          {:halt, :invalid}
        end
      end)

    if is_map(indexed) and map_size(indexed) > 0,
      do: {:ok, indexed}, else: :error
  end

  defp validated_argument(
         %{status: :candidate, source: :original_syntax_and_dictionary_pos,
           construction: :recipient_object, predicate: pred,
           predicate_surface: verb, recipient: rec, object_head: obj,
           object_determiner: det, object_modifier: modifier,
           object_surface: object, object_norm: norm,
           object_status: obj_status, evidence: evidence,
           meaning_verified?: false, event_verified?: false},
         roles, unknown, surfaces)
       when is_integer(pred) and is_integer(rec) and is_integer(det) and
            is_integer(obj) and is_list(evidence) and is_binary(verb) and
            is_binary(object) and is_binary(norm) do
    modifiers_ok? =
      case modifier do
        nil -> obj == det + 1
        n when is_integer(n) -> n == det + 1 and obj == n + 1
        _ -> false
      end

    object_ok? =
      case obj_status do
        :lexically_unresolved_code_head ->
          Enum.any?(unknown, fn
            %{token_index: ^obj, surface: ^object,
              lexically_resolved?: false} -> true
            _ -> false
          end)
        :dictionary_noun_witness -> true
        _ -> false
      end

    required = [:supported_predicate_role, :bounded_argument_order,
                :pronoun_recipient_witness, :determiner_witness, :nominal_head_surface]

    structural? =
      rec == pred + 1 and det == rec + 1 and modifiers_ok? and
        Enum.all?(required, &(&1 in evidence)) and
        Map.get(surfaces, pred) == verb and
        Map.get(surfaces, obj) == object and
        String.downcase(object) == norm and
        is_binary(Map.get(surfaces, rec)) and
        is_binary(Map.get(surfaces, det)) and
        role_supported?(Map.get(roles, pred), pred, :predicate, "verb") and
        (is_nil(modifier) or
           role_supported?(Map.get(roles, modifier), modifier, :modifier, "adjective"))

    if structural? and object_ok? do
      {:ok, %{
        predicate: verb,
        recipient: Map.get(surfaces, rec),
        requested_object: object,
        object_modifier: if(is_integer(modifier), do: Map.get(surfaces, modifier), else: nil),
        object_lexically_resolved?: obj_status == :dictionary_noun_witness
      }}
    else
      :error
    end
  end

  defp validated_argument(_, _, _, _), do: :error

  defp validated_binding([%{
         status: :candidate, source: :original_syntax_and_dictionary_pos,
         frame: :requested_artifact_named_module,
         requested_object_head: obj, requested_object_surface: object,
         container_head: container, container_surface: "module",
         container_name_index: name_index, container_name_surface: name,
         evidence: evidence, meaning_verified?: false,
         semantic_type_verified?: false, existence_verified?: false,
         applied_to_lifg?: false
       }], argument, surfaces, unknown)
       when is_integer(obj) and is_integer(container) and is_integer(name_index) and
            is_binary(object) and is_binary(name) and is_list(evidence) do
    required = [:adjacent_prepositional_attachment, :attested_module_head,
                :attested_naming_marker, :unresolved_identifier_surface]

    if obj == Map.get(argument, :object_head) and
         object == Map.get(surfaces, obj) and
         container == obj + 3 and name_index == obj + 5 and
         Enum.all?(required, &(&1 in evidence)) and
         Enum.map((obj + 1)..name_index, &Map.get(surfaces, &1)) ==
           ["in", "a", "module", "called", name] and
         Enum.any?(unknown, fn
           %{token_index: ^name_index, surface: ^name,
             kind: :declared_identifier, lexically_resolved?: false} -> true
           _ -> false
         end) do
      {:ok, name}
    else
      :none
    end
  end

  defp validated_binding(_, _, _, _), do: :none

  defp role_supported?(%{status: :supported, token_index: index, role: role,
                         pos: pos, evidence: evidence}, index, role, pos)
       when is_list(evidence),
       do: :dictionary_pos_witness in evidence
  defp role_supported?(_, _, _, _), do: false

  defp strip_terminal(value), do: String.replace(value, ~r/[?!.;,]+\z/u, "")
end
