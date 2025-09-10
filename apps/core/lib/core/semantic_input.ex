defmodule Core.SemanticInput do
  @moduledoc """
  Master input state passed through the semantic pipeline.
  """

  alias Core.Token, as: Tok

  @type t :: %__MODULE__{
          sentence: binary(),
          original_sentence: binary() | nil,
          source: atom(),
          tokens: [Tok.t()],      # canonical token list
          pos_list: list(),
          intent: atom() | nil,
          keyword: binary() | nil,
          confidence: float(),
          mood: atom(),
          meta: map()
        }

  @enforce_keys [:sentence, :source]
  defstruct sentence: "",
            original_sentence: nil,
            source: :ui,
            tokens: [],
            pos_list: [],
            intent: nil,
            keyword: nil,
            confidence: 0.0,
            mood: :neutral,
            meta: %{}

  @doc """
  Build a SemanticInput; optionally tokenize immediately.

  Options:
    * `:source`     (atom, default: :ui)
    * `:tokenize`   (bool, default: true)
    * `:token_opts` (keyword, default: [normalize: :phrase, keep_punct: true])
  """
  @spec new(binary(), keyword()) :: t()
  def new(sentence, opts \\ []) when is_binary(sentence) do
    source     = Keyword.get(opts, :source, :ui)
    do_token   = Keyword.get(opts, :tokenize, true)
    token_opts = Keyword.get(opts, :token_opts, [normalize: :phrase, keep_punct: true])

    norm = normalize(sentence)

    base = %__MODULE__{
      sentence: norm,
      original_sentence: sentence,
      source: source
    }

    if do_token and Code.ensure_loaded?(Core.Token) and function_exported?(Core.Token, :tokenize, 2) do
      tokens = Core.Token.tokenize(norm, token_opts)
      put_tokens(base, tokens)
    else
      base
    end
  end

  @doc """
  Set tokens on the struct. Accepts [%Core.Token{}] or [binary] and normalizes to structs.
  """
  @spec put_tokens(t(), list()) :: t()
  def put_tokens(%__MODULE__{} = si, tokens) when is_list(tokens) do
    structs =
      Enum.map(tokens, fn
        %Tok{} = t -> t
        t when is_binary(t) ->
          lower = String.downcase(t)
          %Tok{
            text: t, phrase: lower, lower: lower,
            kind: :word, pos: nil,
            start: 0, stop: byte_size(t),
            keyword: nil, features: %{}, cell: nil
          }
      end)

    %__MODULE__{si | tokens: structs}
  end

  @doc """
  Convenience: get the list of token texts (for logging/counters/UI).
  """
  @spec token_texts(t()) :: [binary()]
  def token_texts(%__MODULE__{tokens: toks}) do
    Enum.map(toks, fn
      %Tok{text: t} -> t
      t when is_binary(t) -> t
    end)
  end

  # ——— private ———
  defp normalize(raw) do
    raw
    |> String.trim_trailing()
    |> String.replace(~r/(?:\r\n|\r|\n){3,}/, "\n\n")
  end
end

