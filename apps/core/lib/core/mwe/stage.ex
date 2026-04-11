defmodule Core.MWE.Stage do
  @moduledoc """
  MWE signature stage for `Core.SemanticInput`.
  """

  alias Core.MWE.Signatures

  @spec run(map(), atom(), keyword()) :: map()
  def run(%{} = si, stage, opts) when is_atom(stage) and is_list(opts) do
    env_on = Application.get_env(:core, :mwe_signatures, :on) != :off
    opt_val = Keyword.get(opts, :mwe_signatures, :inherit)

    enabled =
      case opt_val do
        :off -> false
        false -> false
        :inherit -> env_on
        _ -> true
      end

    if enabled do
      Signatures.run(si,
        stage: stage,
        extra_lex: Keyword.get(opts, :mwe_extra_lex, []),
        multiplier: Keyword.get(opts, :mwe_multiplier, 1.0),
        demote_funcs?: Keyword.get(opts, :mwe_demote_funcs?, true)
      )
    else
      si
    end
  end

  def run(si, _stage, _opts), do: si
end
