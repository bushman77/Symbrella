defmodule LlmTest do
  use ExUnit.Case
  doctest Llm

  test "greets the world" do
    assert Llm.hello() == :world
  end

  test "stop_llama terminates the OS process started by start_llama" do
    name = :"llm_test_#{System.unique_integer([:positive])}"

    tmp_dir =
      Path.join(System.tmp_dir!(), "symbrella-llm-test-#{System.unique_integer([:positive])}")

    File.mkdir_p!(tmp_dir)

    model_path = Path.join(tmp_dir, "model.gguf")
    server_path = Path.join(tmp_dir, "fake_llama_server.py")

    File.write!(model_path, "fake model")
    File.write!(server_path, fake_llama_server_script())
    File.chmod!(server_path, 0o755)

    {:ok, pid} =
      Llm.start_link(
        name: name,
        auto_start_on_boot?: false,
        allow_lazy_start?: false,
        auto_restart_on_crash?: false,
        heartbeat_ms: 0,
        model_path: model_path,
        llama_server: server_path,
        timeout: 5_000,
        port: 0
      )

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf!(tmp_dir)
    end)

    assert {:ok, %{endpoint: endpoint, model_path: ^model_path}} =
             Llm.start_llama(name: name, timeout: 5_000)

    assert String.starts_with?(endpoint, "http://127.0.0.1:")

    assert {:ok, %{status: :ready, runner_os_pid: os_pid, reachable?: true}} =
             Llm.status(name: name)

    child_pid = read_child_pid!(model_path <> ".child_pid")

    assert is_integer(os_pid)
    assert os_pid_alive?(os_pid)
    assert os_pid_alive?(child_pid)

    assert :ok = Llm.stop_llama(name: name, timeout: 5_000)
    refute eventually_os_pid_alive?(os_pid, 20)
    refute eventually_os_pid_alive?(child_pid, 20)

    assert {:ok, %{status: :stopped, runner_os_pid: nil, endpoint: nil, manual_stop?: true}} =
             Llm.status(name: name)
  end

  defp read_child_pid!(path), do: read_child_pid!(path, 20)

  defp read_child_pid!(path, 0) do
    path
    |> File.read!()
    |> String.trim()
    |> String.to_integer()
  end

  defp read_child_pid!(path, attempts) do
    if File.exists?(path) do
      read_child_pid!(path, 0)
    else
      Process.sleep(50)
      read_child_pid!(path, attempts - 1)
    end
  end

  defp os_pid_alive?(os_pid) when is_integer(os_pid) do
    case System.cmd("kill", ["-0", Integer.to_string(os_pid)], stderr_to_stdout: true) do
      {_output, 0} -> true
      {_output, _status} -> false
    end
  end

  defp eventually_os_pid_alive?(os_pid, 0), do: os_pid_alive?(os_pid)

  defp eventually_os_pid_alive?(os_pid, attempts) do
    if os_pid_alive?(os_pid) do
      Process.sleep(50)
      eventually_os_pid_alive?(os_pid, attempts - 1)
    else
      false
    end
  end

  defp fake_llama_server_script do
    """
    #!/usr/bin/env python3
    import argparse
    import json
    import subprocess
    import signal
    import sys
    from http.server import BaseHTTPRequestHandler, HTTPServer

    parser = argparse.ArgumentParser()
    parser.add_argument("-m")
    parser.add_argument("-c")
    parser.add_argument("-t")
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, required=True)
    args, _unknown = parser.parse_known_args()

    child = subprocess.Popen([
        sys.executable,
        "-c",
        "import signal, time; signal.signal(signal.SIGTERM, signal.SIG_IGN); time.sleep(3600)"
    ])

    with open(args.m + ".child_pid", "w", encoding="utf-8") as file:
        file.write(str(child.pid))

    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            if self.path == "/v1/models":
                body = json.dumps({"data": [{"id": "fake-local"}]}).encode("utf-8")
                self.send_response(200)
                self.send_header("content-type", "application/json")
                self.send_header("content-length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)
            else:
                self.send_response(404)
                self.end_headers()

        def log_message(self, format, *args):
            return

    httpd = HTTPServer((args.host, args.port), Handler)

    def stop(_signum, _frame):
        httpd.server_close()
        sys.exit(0)

    signal.signal(signal.SIGTERM, stop)
    httpd.serve_forever()
    """
  end
end
