defmodule Llm.ModelPathConfigTest do
  use ExUnit.Case, async: true

  @config_file Path.expand("../../../../config/llm.exs", __DIR__)

  setup do
    home =
      Path.join(
        System.tmp_dir!(),
        "symbrella-model-env-#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(home)
    on_exit(fn -> File.rm_rf!(home) end)
    %{home: home}
  end

  test "loads model path from the current user's home .env without shell sourcing", %{home: home} do
    File.write!(Path.join(home, ".env"), """
    # Other home settings should not affect the LLM config.
    UNRELATED_SETTING=some-value
    export LLAMA_MODEL_PATH="~/models/mythomax-l2-13b.Q4_K_M.gguf"
    """)

    assert model_path(home) ==
      Path.join(home, "models/mythomax-l2-13b.Q4_K_M.gguf")
  end

  test "explicit shell variable overrides the home .env", %{home: home} do
    File.write!(Path.join(home, ".env"), "LLAMA_MODEL_PATH=~/models/old.gguf\n")

    assert model_path(home, "/tmp/overridden.gguf") == "/tmp/overridden.gguf"
  end

  test "missing home file and empty shell variable leave model unset", %{home: home} do
    assert model_path(home) == "NONE"
  end

  test "last definition wins and empty paths do not substitute a hardcoded model", %{home: home} do
    File.write!(Path.join(home, ".env"), """
    LLAMA_MODEL_PATH=~/models/old.gguf
    LLAMA_MODEL_PATH='~/models/current.gguf'
    """)

    assert model_path(home) == Path.join(home, "models/current.gguf")

    File.write!(Path.join(home, ".env"), "LLAMA_MODEL_PATH=\n")
    assert model_path(home) == "NONE"
  end

  test "optional ignored machine-specific config overrides the shared defaults", %{home: home} do
    config_dir = Path.join(home, "config")
    File.mkdir_p!(config_dir)
    File.cp!(@config_file, Path.join(config_dir, "llm.exs"))

    File.write!(Path.join(config_dir, "llm.local.exs"), """
    import Config
    config :llm, Llm, ctx: 8192, n_gpu_layers: 33
    config :llm, :runner, ctx: 8192
    """)

    code = """
    cfg = Config.Reader.read!(#{inspect(Path.join(config_dir, "llm.exs"))})
    options = cfg |> Keyword.fetch!(:llm) |> Keyword.fetch!(Llm)
    runner = cfg |> Keyword.fetch!(:llm) |> Keyword.fetch!(:runner)
    IO.puts(Enum.join([
      Keyword.fetch!(options, :ctx),
      Keyword.fetch!(options, :n_gpu_layers),
      Keyword.fetch!(runner, :ctx)
    ], ","))
    """

    {output, status} =
      System.cmd("elixir", ["-e", code],
        env: [{"HOME", home}, {"LLAMA_MODEL_PATH", ""}],
        stderr_to_stdout: true
      )

    assert status == 0, output
    assert String.trim(output) == "8192,33,8192"
  end

  defp model_path(home, exported \\ "") do
    # Separate process keeps HOME and LLAMA_MODEL_PATH isolated from umbrella
    # application state and from concurrent test cases.
    code = """
    config = Config.Reader.read!(#{inspect(@config_file)})
    llm_config = config |> Keyword.fetch!(:llm) |> Keyword.fetch!(Llm)
    case Keyword.get(llm_config, :model_path) do
      nil -> IO.puts("NONE")
      path -> IO.puts(path)
    end
    """

    {output, status} =
      System.cmd("elixir", ["-e", code],
        env: [{"HOME", home}, {"LLAMA_MODEL_PATH", exported}],
        stderr_to_stdout: true
      )

    assert status == 0, output
    String.trim(output)
  end
end
