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
  @actors %{"bird" => :bird, "cat" => :cat, "dog" => :dog}
  @actions %{"sitting" => :sit, "sleeping" => :sleep, "flying" => :fly}

  @event_property {:arrow, :event, :truth}
  @spatial_modifier_type {:arrow, :landmark_kind,
    {:arrow, @event_property, @event_property}}

  @nominal_property {:arrow, :entity, :truth}
  @nominal_modifier {:arrow, @nominal_property, @nominal_property}

  @doc "Attested nominal surface mapped to a closed entity property (not a referent)."
  @spec nominal_property(String.t()) :: {:ok, map()} | :unsupported
  def nominal_property(surface) when is_binary(surface) do
    with {:ok, term} <- Expression.noun(surface),
         {:ok, @nominal_property} <- Expression.type_of(term) do
      {:ok, %{surface: surface, term: term, type: @nominal_property}}
    else
      _ -> :unsupported
    end
  end

  def nominal_property(_), do: :unsupported

  @doc "An adjective acts on any compatible nominal property, not a whole phrase."
  @spec nominal_modifier(String.t()) :: {:ok, map()} | :unsupported
  def nominal_modifier(surface) when is_binary(surface) do
    with {:ok, term} <- Expression.adjective(surface),
         {:ok, @nominal_modifier} <- Expression.type_of(term) do
      {:ok, %{surface: surface, term: term, type: @nominal_modifier}}
    else
      _ -> :unsupported
    end
  end

  def nominal_modifier(_), do: :unsupported

  @doc "Closed lexical event-action meaning, without asserting an occurrence."
  @spec action(String.t()) :: {:ok, map()} | :unsupported
  def action(surface) when is_binary(surface) do
    case Map.fetch(@actions, surface) do
      {:ok, symbol} ->
        term = {:action_kind, symbol}
        if Expression.type_of(term) == {:ok, :action_kind},
          do: {:ok, %{surface: surface, symbol: symbol, term: term, type: :action_kind}},
          else: :unsupported
      :error -> :unsupported
    end
  end

  def action(_), do: :unsupported

  @doc "Closed lexical actor-kind meaning, without identifying an individual."
  @spec actor(String.t()) :: {:ok, map()} | :unsupported
  def actor(surface) when is_binary(surface) do
    case Map.fetch(@actors, surface) do
      {:ok, symbol} ->
        term = {:actor_kind, symbol}
        if Expression.type_of(term) == {:ok, :actor_kind},
          do: {:ok, %{surface: surface, symbol: symbol, term: term, type: :actor_kind}},
          else: :unsupported
      :error -> :unsupported
    end
  end

  def actor(_), do: :unsupported

  @doc "Reusable progressive-aspect operator: action-kind -> event -> truth."
  @spec progressive_event() :: {:ok, map()} | :unsupported
  def progressive_event do
    type = {:arrow, :action_kind, @event_property}
    term =
      {:lambda, "action", :action_kind,
       {:lambda, "e", :event,
        {:and, {:event_action, {:var, "action"}, {:var, "e"}},
         {:event_aspect, :progressive, {:var, "e"}}}}}

    if Expression.type_of(term) == {:ok, type},
      do: {:ok, %{term: term, type: type}},
      else: :unsupported
  end

  @doc "Reusable actor operator: actor-kind -> event-property -> event-property."
  @spec actor_modifier() :: {:ok, map()} | :unsupported
  def actor_modifier do
    type = {:arrow, :actor_kind, {:arrow, @event_property, @event_property}}
    term =
      {:lambda, "actor", :actor_kind,
       {:lambda, "P", @event_property,
        {:lambda, "e", :event,
         {:and, {:apply, {:var, "P"}, {:var, "e"}},
          {:event_actor_kind, {:var, "actor"}, {:var, "e"}}}}}}

    if Expression.type_of(term) == {:ok, type},
      do: {:ok, %{term: term, type: type}},
      else: :unsupported
  end

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

  @spec known_action?(atom()) :: boolean()
  def known_action?(symbol) when is_atom(symbol), do: symbol in Map.values(@actions)
  def known_action?(_), do: false

  @spec known_actor?(atom()) :: boolean()
  def known_actor?(symbol) when is_atom(symbol), do: symbol in Map.values(@actors)
  def known_actor?(_), do: false

  @spec known_relation?(atom()) :: boolean()
  def known_relation?(relation) when is_atom(relation),
    do: relation in Map.values(@relations)
  def known_relation?(_), do: false

  @spec known_landmark?(atom()) :: boolean()
  def known_landmark?(kind) when is_atom(kind),
    do: kind in Map.values(@landmarks)
  def known_landmark?(_), do: false
end
