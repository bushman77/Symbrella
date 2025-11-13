defmodule Core.Response.Guardrails do
  @moduledoc """
  Repo-specific guardrail detectors.
  Heuristic, text-only checks to flag risky or disallowed refactors unless approved.

  Flags
  • :acyclic_violation     — mentions that imply db↔brain↔core↔web cycle breakage.
  • :lifg_move             — moving LIFG out of apps/brain.
  • :chargrams_to_lifg     — char-grams flowing to LIFG path.
  • :app_supervisor_rename — renaming core app supervisors (Brain.Application, etc).
  • :unknown               — fallback if phrasing is suspicious but not categorized.

  Approval token
  • Matches: `Approve: P-###` (case-insensitive).
  """

  @approve ~r/\bapprove:\s*p-\d{3}\b/i

  @spec detect(String.t()) :: %{guardrail?: boolean, approve_token?: boolean, flags: [atom]}
  def detect(text) do
    t = dn(text)

    flags =
      []
      |> flag_if(t =~ ~r/\b(db.*brain.*core.*web|web.*core.*brain.*db)\b/)
      |> flag_if(t =~ ~r/\bmove\b.*\bLIFG\b/i, :lifg_move)
      |> flag_if(t =~ ~r/\bchar(-| )?grams?\b.*\bLIFG\b/i, :chargrams_to_lifg)
      |> flag_if(
        t =~ ~r/\brename\b.*(Brain\.Application|Symbrella\.Application)/i,
        :app_supervisor_rename
      )

    flags2 = if flags == [], do: flags, else: flags

    %{
      guardrail?: flags2 != [],
      approve_token?: Regex.match?(@approve, t),
      flags: if(flags2 == [], do: [], else: Enum.uniq(flags2))
    }
  end

  defp flag_if(list, true), do: [:acyclic_violation | list]
  defp flag_if(list, false), do: list
  defp flag_if(list, true, tag), do: [tag | list]
  defp flag_if(list, false, _tag), do: list

  defp dn(nil), do: ""
  defp dn(t), do: t |> to_string() |> String.downcase()
end
