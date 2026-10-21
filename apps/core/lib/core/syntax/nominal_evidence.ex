defmodule Core.Syntax.NominalEvidence do
  @moduledoc """
  Read-only nominal structure for lexical gaps in technical expressions.

  An unresolved CamelCase surface may occupy a noun-phrase head position
  without becoming a dictionary noun sense. Only an attested determiner
  followed by an attested adjective licenses modifier POS support.

  All positions refer to original, single-token indices. A code-shaped word
  alone supplies neither a lexical meaning nor a semantic fact.
  """

  @determiners MapSet.new(~w(a an the this that these those my your his her its our their))
  @max_tokens 64

  @spec detect(map()) :: map()
  def detect(%{} = si) do
    tokens = List.wrap(Map.get(si, :tokens, []))
    cells = List.wrap(Map.get(si, :active_cells, []))
    nodes = tokens
      |> Enum.filter(&(is_map(&1) and Map.get(&1, :n, 1) == 1))
      |> Map.new(fn token ->
        {Map.get(token, :index), Map.get(token, :phrase)}
      end)

    if map_size(nodes) > @max_tokens do
      empty()
    else
      unresolved =
        nodes
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.flat_map(fn {idx, raw} ->
          word = strip_terminal_punctuation(raw)

          if code_identifier?(word) and not lexically_witnessed?(cells, word) do
            context = if normalize(Map.get(nodes, idx - 1)) == "called" and
                normalize(Map.get(nodes, idx - 2)) in ["module", "class", "function"],
              do: :declared_identifier, else: :technical_identifier

            [%{token_index: idx, surface: word, kind: context,
               lexically_resolved?: false, meaning_verified?: false,
               source: :original_token_surface}]
          else
            []
          end
        end)

      frames =
        unresolved
        |> Enum.flat_map(fn %{token_index: head} = identifier ->
          det = head - 2
          modifier = head - 1

          if MapSet.member?(@determiners, normalize(Map.get(nodes, det))) and
               witnessed?(cells, Map.get(nodes, det), "determiner") and
               witnessed?(cells, Map.get(nodes, modifier), "adjective") do
            [%{span: {det, head}, determiner: det, modifier: modifier,
               head: head, head_surface: identifier.surface,
               head_status: :lexically_unresolved,
               frame: :determiner_adjective_unresolved_head,
               source: :original_surface_and_dictionary_pos}]
          else
            []
          end
        end)

      proposals = Enum.map(frames, fn frame ->
        %{token_index: frame.modifier, pos: "adjective", role: :modifier,
          frame: :unresolved_nominal, span: frame.span,
          basis: :determined_adjective_before_unresolved_head,
          evidence: [:bounded_determiner_surface, :dictionary_pos_witness,
                     :unresolved_code_head, :complete_nominal_surface]}
      end)

      %{frames: frames, proposals: proposals, unresolved_tokens: unresolved}
    end
  end

  def detect(_), do: empty()

  defp empty, do: %{frames: [], proposals: [], unresolved_tokens: []}

  defp witnessed?(cells, raw, pos) do
    word = normalize(raw)
    is_binary(word) and word != "" and
      Enum.any?(cells, fn
        %{} = cell ->
          normalize(Map.get(cell, :norm) || Map.get(cell, :word)) == word and
            normalize(Map.get(cell, :pos)) == pos and
            case Map.get(cell, :id) do
              id when is_binary(id) ->
                String.starts_with?(id, word <> "|" <> pos <> "|")
              _ -> false
            end
        _ -> false
      end)
  end

  defp lexically_witnessed?(cells, word) do
    Enum.any?(cells, fn
      %{} = cell ->
        normalize(Map.get(cell, :norm) || Map.get(cell, :word)) ==
          normalize(word) and is_binary(Map.get(cell, :id))
      _ -> false
    end)
  end

  defp code_identifier?(word) when is_binary(word) do
    byte_size(word) <= 96 and
      Regex.match?(~r/\A[A-Z][a-z0-9]+(?:[A-Z][a-z0-9]+)+\z/, word)
  end

  defp code_identifier?(_), do: false

  defp strip_terminal_punctuation(word) when is_binary(word) do
    String.replace(word, ~r/[?!.;,]+\z/u, "")
  end

  defp strip_terminal_punctuation(_), do: ""

  defp normalize(word) when is_binary(word) do
    word
    |> strip_terminal_punctuation()
    |> String.downcase()
  end

  defp normalize(_), do: nil
end
