defmodule Core.Recall.Synonyms.Provider do
  @moduledoc """
  Behaviour for synonym providers. Implement this to plug in DB/lexicon sources.

  Return **normalized** entries (lowercase/NFC), we’ll handle ranking/merge.

  Expected shape per entry:
    %{
      lemma:  "word",            # required
      pos:    :noun | :verb | nil,
      prior:  0.0..1.0,          # a prior/quality score
      source: "provider_name",   # e.g. "wordnet", "db", "fallback"
      meta:   map()              # optional provider-specific fields
    }
  """

  @type word :: String.t()
  @type pos :: atom() | nil
  @type entry :: %{lemma: word(), pos: pos(), prior: float(), source: String.t(), meta: map()}

  @callback lookup(word(), pos(), keyword()) ::
              {:ok, [entry()], map()} | {:ok, [entry()]} | {:error, term()}
end
