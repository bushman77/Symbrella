defmodule Core.Token do
  defstruct [
    :text,            # The token itself (e.g. "run", "apple")
    :input_text,      # Original input sentence
    :phrase,          # Phrase this token is part of (if any)
    :is_phrase,       # Boolean: is this token a multi-word phrase?
    :index,           # Index position in sentence
    :pos,             # Part of speech tag, e.g., "NOUN"
    :position,        # Coordinates or position vector if used
    :embedding,       # Vector embedding (if available)
    :embedded_vector, # Optional alias or alternative for embedding
    :response,        # Optional phrase or sentence response for this token
    :source           # :tokenizer, :enricher, etc.
  ]
end

