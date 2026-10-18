defmodule Core.Tools.WebRetrieval.PlaceScope do
  @moduledoc """
  Read-only, same-session jurisdiction *lookup hint* for an ambiguous
  two-place distance question.

  A previous USER question can explicitly qualify one of the exact places
  ("where is Kamloops BC"). A short-lived hint may then scope BOTH current
  unqualified names to that province for coordinate provider lookup ONLY.
  The original symbolic frame is untouched; provider identity, jurisdiction
  and coordinates must all still pass their ordinary validation.

  No assistant reply, LLM assertion, dictionary selection, or remembered
  fact is used as geographic evidence. Contradictory user hints abstain.
  """

  @max_history_pairs 3
  @province_codes %{
    "bc" => "BC", "british columbia" => "BC",
    "ab" => "AB", "alberta" => "AB",
    "on" => "ON", "ontario" => "ON",
    "qc" => "QC", "quebec" => "QC"
  }

  @explicit_place ~r/^\s*(?:where\s+is|what\s+is|tell\s+me\s+about)\s+([\p{L}][\p{L}\p{M}' -]{1,80}?)\s*(?:,\s*|\s+)(BC|British\s+Columbia|AB|Alberta|ON|Ontario|QC|Quebec)\s*[?.!]*\s*$/iu

  @spec for_lookup(map(), list()) :: map()
  def for_lookup(%{} = context, history) when is_list(history) do
    case current_frame(context) do
      %{type: :location_question, relation: :proximity,
        subject: subject, target: target,
        grounding_status: :unverified} = frame
      when is_binary(subject) and is_binary(target) ->
        with false <- qualified?(subject) or qualified?(target),
             {:ok, region} <- region_hint(history, subject, target) do
          lookup_frame =
            frame
            |> Map.put(:subject, subject <> ", " <> region)
            |> Map.put(:target, target <> ", " <> region)

          context
          |> Map.put(:geographic_lookup_scope, %{
            status: :user_qualified_hint,
            region: region,
            verified?: false,
            provider_must_verify?: true
          })
          |> Map.put(:symbolic_frame, %{event: lookup_frame})
          |> Map.update(:features, %{symbolic_frame: lookup_frame}, fn
            %{} = features -> Map.put(features, :symbolic_frame, lookup_frame)
            _ -> %{symbolic_frame: lookup_frame}
          end)
        else
          _ -> context
        end

      _ -> context
    end
  end

  def for_lookup(context, _), do: context

  @spec scope_summary(map()) :: map() | nil
  def scope_summary(%{geographic_lookup_scope: scope})
      when is_map(scope),
      do: Map.take(scope, [:status, :region, :verified?,
                           :provider_must_verify?])

  def scope_summary(_), do: nil

  defp region_hint(history, subject, target) do
    hints =
      history
      |> Enum.filter(fn
        %{"role" => "user", "content" => text}
        when is_binary(text) -> true
        _ -> false
      end)
      |> Enum.take(-@max_history_pairs)
      |> Enum.flat_map(fn %{"content" => text} ->
        case Regex.run(@explicit_place, text) do
          [_, named, region] ->
            if same_name?(named, subject) or same_name?(named, target) do
              case Map.get(@province_codes, normalize(region)) do
                nil -> []
                code -> [code]
              end
            else
              []
            end

          _ -> []
        end
      end)
      |> Enum.uniq()

    case hints do
      [single] -> {:ok, single}
      _ -> :no_hint
    end
  end

  defp current_frame(context) do
    case get_in(context, [:features, :symbolic_frame]) do
      %{type: :location_question} = frame -> frame
      _ -> get_in(context, [:symbolic_frame, :event])
    end
  end

  defp qualified?(name) do
    Regex.match?(
      ~r/(?:,\s*|\s+)(?:BC|British\s+Columbia|AB|Alberta|ON|Ontario|QC|Quebec)\s*$/iu,
      name
    )
  end

  defp same_name?(a, b),
    do: normalize(a) != "" and normalize(a) == normalize(b)

  defp normalize(s) do
    s
    |> String.downcase()
    |> String.replace(~r/[^\p{L}\p{M}]+/u, " ")
    |> String.trim()
  end
end
