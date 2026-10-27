defmodule Core.Semantic.Composition.Lexical do
  @moduledoc """
  Bounded, pure lexical denotations for incremental typed composition.

  Lexical surfaces supply closed semantic constants and lambda expressions,
  never executable functions or factual assertions. A successful lookup does
  not decide a dictionary sense: Composition.RoleWitness independently checks
  the current-turn lexical evidence before these terms can enter a proof.
  """

  alias Core.Semantic.Composition.Expression

  @relations %{
    "on" => :on, "in" => :in, "under" => :under, "beside" => :beside,
    "above" => :above, "below" => :below, "into" => :into
  }
  @landmarks %{"fence" => :fence, "tree" => :tree, "table" => :table}

  @event_property {:arrow, :event, :truth}
  @spatial_modifier_type {:arrow, :landmark_kind,
    {:arrow, @event_property, @event_property}}

  @doc """
  A spatial preposition denotes a function of a landmark and an event
  property, returning a new event property. The landmark is not baked in.
  """
  @spec spatial_modifier(String.t()) :: {:ok, map()} | :unsupported
  def spatial_modifier(surface) when is_binary(surface) do
    case Map.fetch(@relations, surface) do
      {:ok, relation} ->
        term =
          {:lambda, "landmark", :landmark_kind,
           {:lambda, "P", @event_property,
            {:lambda, "e", :event,
             {:and, {:apply, {:var, "P"}, {:var, "e"}},
              {:event_spatial, relation, {:var, "landmark"}, {:var, "e"}}}}}}

        case Expression.type_of(term) do
          {:ok, @spatial_modifier_type} ->
            {:ok, %{surface: surface, symbol: relation, term: term,
                    type: @spatial_modifier_type}}

          _ ->
            :unsupported
        end

      :error ->
        :unsupported
    end
  end

  def spatial_modifier(_), do: :unsupported

  @doc "A closed landmark-kind constant; neither a referent nor a fact."
  @spec landmark(String.t()) :: {:ok, map()} | :unsupported
  def landmark(surface) when is_binary(surface) do
    case Map.fetch(@landmarks, surface) do
      {:ok, kind} ->
        term = {:landmark_kind, kind}
        case Expression.type_of(term) do
          {:ok, :landmark_kind} ->
            {:ok, %{surface: surface, symbol: kind, term: term,
                    type: :landmark_kind}}

          _ ->
            :unsupported
        end

      :error ->
        :unsupported
    end
  end

  def landmark(_), do: :unsupported

  @spec known_relation?(atom()) :: boolean()
  def known_relation?(relation) when is_atom(relation),
    do: relation in Map.values(@relations)
  def known_relation?(_), do: false

  @spec known_landmark?(atom()) :: boolean()
  def known_landmark?(kind) when is_atom(kind),
    do: kind in Map.values(@landmarks)
  def known_landmark?(_), do: false
end
