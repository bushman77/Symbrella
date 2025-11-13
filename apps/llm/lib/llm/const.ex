# apps/llm/lib/llm/const.ex
defmodule Llm.Const do
  @moduledoc false

  @stable_runner_opts %{
    num_ctx: 1024,
    top_k: 1,
    top_p: 1.0,
    repeat_penalty: 1.0,
    seed: 42,
    num_predict: 80
  }
  @default_keep_alive "10m"

  def stable_runner_opts, do: @stable_runner_opts
  def default_keep_alive, do: @default_keep_alive
end
