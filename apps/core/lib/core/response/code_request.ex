defmodule Core.Response.CodeRequest do
  @moduledoc """
  Narrow, bounded completion contract for a witnessed request to author a
  basic Elixir GenServer in an explicitly named module.

  The current-turn request structure supplies the module identifier; it is
  not a verified dictionary meaning or evidence that the code ran.
  Revisions and the static fallback are expression-only, with no execution,
  source writes, BrainCell stimulation, or episodic recording.
  """

  @module_pattern ~r/\A[A-Z][A-Za-z0-9]{0,79}\z/
  @request_terms ~r/\b(?:write|create|build|make|generate|implement|show)\b/iu

  @spec eligible?(map()) :: boolean()
  def eligible?(%{} = context) do
    structure = Map.get(context, :request_structure)
    features = Map.get(context, :features, %{})
    text = Map.get(context, :user_text, "")

    match?(
      %{
        version: 1, status: :grammatical_candidate,
        source: :current_turn_original_tokens,
        predicate: "write", requested_object: "GenServer",
        object_lexically_resolved?: false,
        meaning_verified?: false, code_semantics_verified?: false
      },
      structure
    ) and
      is_map(features) and
      Map.get(features, :selected_action) == :answer_user and
      Map.get(features, :guardrail?) != true and
      Map.get(features, :risk_bucket) != :high and
      is_binary(text) and Regex.match?(@request_terms, text) and
      valid_module_name?(module_name(structure)) and
      String.contains?(text, "GenServer") and
      String.contains?(text, module_name(structure))
  end

  def eligible?(_), do: false

  @spec eligible_structure?(map() | nil, String.t()) :: boolean()
  def eligible_structure?(structure, text) when is_map(structure) and is_binary(text) do
    eligible?(%{request_structure: structure, user_text: text,
      features: %{selected_action: :answer_user, guardrail?: false,
        risk_bucket: :low}})
  end

  def eligible_structure?(_, _), do: false

  @spec complete?(map(), String.t()) :: boolean()
  def complete?(context, draft) when is_binary(draft) do
    if eligible?(context) do
      name = module_name(Map.get(context, :request_structure))
      # Require a real source-shaped definition, not a paraphrase or
      # promise. The model may use 'use GenServer' or '@behaviour GenServer'.
      module_def = Regex.compile!("\\bdefmodule\\s+" <> Regex.escape(name) <> "\\s+do\\b")
      Regex.match?(module_def, draft) and
        Regex.match?(~r/\b(?:use\s+GenServer|@behaviour\s+GenServer)\b/u, draft) and
        Regex.match?(~r/\bdef\s+(?:init|start_link)\b/u, draft) and
        Regex.match?(~r/\bend\b/u, draft)
    else
      false
    end
  end

  def complete?(_, _), do: false

  @spec missing?(map(), String.t()) :: boolean()
  def missing?(context, draft),
    do: eligible?(context) and not complete?(context, draft)

  @spec revision_instruction(map()) :: String.t()
  def revision_instruction(%{} = context) do
    if eligible?(context) do
      name = module_name(Map.get(context, :request_structure))

      "The last answer offered to help or asked for unnecessary details " <>
        "instead of fulfilling this explicit, safe code request. " <>
        "Write a minimal working Elixir GenServer source example in " <>
        "defmodule #{name} do ... end, including use GenServer, " <>
        "start_link/1 and init/1. A basic initial-state server is enough; " <>
        "don't invent extra application requirements. Supply actual code, " <>
        "not a promise, menu of options or a clarification question. " <>
        "Do not claim you executed, saved or tested the code."
    else
      ""
    end
  end

  @spec fallback(map()) :: String.t() | nil
  def fallback(%{} = context) do
    if eligible?(context) do
      name = module_name(Map.get(context, :request_structure))

      """
      Here's a minimal GenServer named `#{name}`:

      ```elixir
      defmodule #{name} do
        use GenServer

        def start_link(initial_state \\\\ %{}) do
          GenServer.start_link(__MODULE__, initial_state, name: __MODULE__)
        end

        @impl true
        def init(initial_state), do: {:ok, initial_state}
      end
      ```

      Start it with `#{name}.start_link()`. This is a basic template, not executed or tested code.
      """
    else
      nil
    end
  end

  def fallback(_), do: nil

  defp module_name(%{module_name: name}) when is_binary(name), do: name
  defp module_name(_), do: nil

  defp valid_module_name?(name) when is_binary(name) do
    byte_size(name) <= 80 and Regex.match?(@module_pattern, name)
  end

  defp valid_module_name?(_), do: false
end
