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

  NOTE:
  Core.Response.Attach optionally calls guardrails as a *response-text* pass.
  To keep the pipeline stable, we provide run/enforce/apply as no-op passthroughs.
  """

  @approve ~r/\bapprove:\s*p-\d{3}\b/i

  @spec detect(String.t()) :: %{guardrail?: boolean, approve_token?: boolean, flags: [atom]}
  def detect(text) do
    t = dn(text)

    flags =
      []
      |> flag_if(t =~ ~r/\b(db.*brain.*core.*web|web.*core.*brain.*db)\b/, :acyclic_violation)
      |> flag_if(t =~ ~r/\bmove\b.*\blifg\b/i, :lifg_move)
      |> flag_if(t =~ ~r/\bchar(-| )?grams?\b.*\blifg\b/i, :chargrams_to_lifg)
      |> flag_if(
        t =~ ~r/\brename\b.*(Brain\.Application|Symbrella\.Application)/i,
        :app_supervisor_rename
      )

    %{
      guardrail?: flags != [],
      approve_token?: Regex.match?(@approve, t),
      flags: Enum.uniq(flags)
    }
  end

  # Optional response guardrail hooks (no-op for now).
  # Expected shapes:
  #   run/enforce/apply(text, ctx, meta) -> {text, meta} OR {tone, text, meta}
  @spec run(String.t(), map(), map()) :: {String.t(), map()}
  def run(text, _ctx, meta), do: {text, ensure_map(meta)}

  @spec enforce(String.t(), map(), map()) :: {String.t(), map()}
  def enforce(text, ctx, meta), do: run(text, ctx, meta)

  @spec apply(String.t(), map(), map()) :: {String.t(), map()}
  def apply(text, ctx, meta), do: run(text, ctx, meta)

  defp flag_if(list, true, tag), do: [tag | list]
  defp flag_if(list, false, _tag), do: list

  defp ensure_map(%{} = m), do: m
  defp ensure_map(_), do: %{}

  defp dn(nil), do: ""
  defp dn(t), do: t |> to_string() |> String.downcase()
end

