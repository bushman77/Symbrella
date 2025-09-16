defmodule Core.PhraseRepo do
  @moduledoc "Behaviour for phrase existence checks."
  @callback exists?(phrase :: String.t()) :: boolean()
end
