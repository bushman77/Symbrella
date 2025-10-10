# apps/core/lib/core/recall/synonyms/source.ex
defmodule Core.Recall.Synonyms.Source do
  @moduledoc """
  Behaviour for synonym sources used by the recall layer.

  A source returns **grouped** rows keyed by the requested norms.
  The concrete `Core.Recall.Synonyms` module will extract `synonyms`
  from these rows and build recall refs.
  """

  @type norm :: binary()
  @type row :: map()

  @callback fetch_grouped([norm()]) :: %{optional(norm()) => [row()]}
end

