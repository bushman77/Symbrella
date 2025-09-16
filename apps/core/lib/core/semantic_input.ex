defmodule Core.SemanticInput do
  @moduledoc """
  Pipeline state passed through Core.

  Minimal fields for early pipeline (tokenize → runtime_bind).
  Safe to grow over time; downstream stages can rely on presence of
  pos_list/intent/keyword/confidence/mood/etc.
  """

  @enforce_keys [:sentence]
  defstruct original_sentence: "",
            sentence: "",
            source: :user,
            tokens: [],
            pos_list: [],
            intent: nil,
            keyword: nil,
            confidence: 0.0,
            mood: nil,
            phrase_matches: [],
            activation_summary: %{},
            pattern_roles: %{},
            active_cells: [],
            brain_state_ref: nil,
            trace: []

  @type source :: :user | :system | :api | :internal | :recall

  @type tok :: Core.Token.t()
          | %{
              id: non_neg_integer(),
              text: binary(),
              norm: binary(),
              lemma: binary() | nil,
              span: %{char_start: non_neg_integer(), char_end: non_neg_integer()},
              is_mwe_head: boolean(),
              mwe_id: binary() | nil,
              mwe_span: %{first_id: non_neg_integer(), last_id: non_neg_integer()} | nil,
              is_stop: boolean(),
              is_oov: boolean(),
              kind: :word | :punct | :number | :emoji | :other,
              hash: binary()
            }

  @type tok_ref :: %{tok_id: non_neg_integer(), conf: float() | nil}

  @type active_ref :: %{
          id: term(),
          matched_tokens: [tok_ref()],
          activation_snapshot: float(),
          source: :runtime | :recency | :db_recall,
          reason: :matched_active_cell | :recent_activation | :exact | :synonym | :embedding,
          score: float() | nil,
          ts_ms: non_neg_integer()
        }

  @type event :: %{stage: atom(), ts_ms: non_neg_integer(), meta: map()}

  @type t :: %__MODULE__{
          original_sentence: binary(),
          sentence: binary(),
          source: source(),
          tokens: [tok()],
          pos_list: [String.t()],
          intent: atom() | String.t() | nil,
          keyword: String.t() | nil,
          confidence: float(),
          mood: atom() | nil,
          phrase_matches: list(),
          activation_summary: map(),
          pattern_roles: map(),
          active_cells: [active_ref()],
          brain_state_ref: term() | nil,
          trace: [event()]
        }
end
