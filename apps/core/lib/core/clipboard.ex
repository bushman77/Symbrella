defmodule Core.Clipboard do
  @moduledoc """
  Small development helper for copying Elixir terms to the desktop clipboard.
  """

  @spec copy(term(), keyword()) :: term()
  def copy(term, opts \\ []) do
    text =
      inspect(term,
        pretty: Keyword.get(opts, :pretty, true),
        width: Keyword.get(opts, :width, 120),
        limit: Keyword.get(opts, :limit, :infinity),
        printable_limit: Keyword.get(opts, :printable_limit, :infinity)
      )

    copy_text!(text)
    term
  end

  @spec copy_text!(String.t()) :: :ok
  def copy_text!(text) when is_binary(text) do
    xclip =
      System.find_executable("xclip") ||
        raise "xclip is not installed or not available in PATH"

    tmp =
      Path.join(
        System.tmp_dir!(),
        "symbrella_clipboard_#{System.unique_integer([:positive, :monotonic])}.txt"
      )

    File.write!(tmp, text)

    case System.cmd(
           "sh",
           [
             "-c",
             ~S("$1" -selection clipboard < "$2" >/dev/null 2>&1 &),
             "symbrella-xclip",
             xclip,
             tmp
           ]
         ) do
      {_output, 0} ->
        # Give xclip a moment to open/read the temp file before deleting it.
        Process.sleep(50)
        File.rm(tmp)
        :ok

      {output, status} ->
        File.rm(tmp)
        raise "xclip failed with status #{status}: #{String.trim(output)}"
    end
  end
end
