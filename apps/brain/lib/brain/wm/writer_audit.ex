defmodule Brain.WM.WriterAudit do
  @moduledoc """
  Read-only, AST-based inventory of the known production Working Memory writers.

  Enforces a *review list*, not a security sandbox: dynamic calls, generated
  code, remote BEAM state replacement and unrecognized mutation idioms may
  evade this scan. Runtime evidence/bypass tests remain mandatory for V1.
  """

  @expected %{
    "apps/brain/lib/brain.ex" => %{map_update: 4},
    "apps/brain/lib/brain/pipeline/lifg_stage1.ex" => %{map_put: 1},
    "apps/brain/lib/brain/wm/gate.ex" => %{map_put: 1},
    "apps/brain/lib/brain/wm/policy.ex" => %{map_put: 2},
    "apps/brain/lib/brain/wm/admission.ex" => %{working_memory_upsert: 2}
  }

  @doc "Reviewed mutation counts by production module. Changes require explicit audit."
  def expected, do: @expected

  @doc "AST inspection of a single source. No application processes are started."
  @spec source_events(String.t()) :: {:ok, [map()]} | {:error, term()}
  def source_events(source) when is_binary(source) do
    with {:ok, ast} <- Code.string_to_quoted(source) do
      {_ast, events} =
        Macro.prewalk(ast, [], fn node, acc ->
          case event_kind(node) do
            nil -> {node, acc}
            kind -> {node, [%{kind: kind, line: node_line(node)} | acc]}
          end
        end)

      {:ok, Enum.reverse(events)}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def source_events(_), do: {:error, :invalid_source}

  @doc "Discover recognized WM writers across ALL production umbrella lib .ex files."
  @spec inventory(String.t()) :: {:ok, [map()]} | {:error, term()}
  def inventory(root \\ default_root()) do
    files =
      root
      |> Path.join("apps/*/lib/**/*.ex")
      |> Path.wildcard()
      |> Enum.sort()

    if files == [] do
      {:error, :no_production_elixir_sources}
    else
      Enum.reduce_while(files, {:ok, []}, fn file, {:ok, acc} ->
        with {:ok, content} <- File.read(file),
             {:ok, events} <- source_events(content) do
          relative = Path.relative_to(file, root)
          tagged = Enum.map(events, &Map.put(&1, :path, relative))
          {:cont, {:ok, tagged ++ acc}}
        else
          {:error, reason} -> {:halt, {:error, {file, reason}}}
        end
      end)
      |> case do
        {:ok, events} ->
          {:ok, Enum.sort_by(events, &{&1.path, &1.line, &1.kind})}

        other ->
          other
      end
    end
  end

  @doc "Fail when a previously unreviewed writer appears or a reviewed one disappears."
  @spec verify(String.t()) :: {:ok, map()} | {:error, term()}
  def verify(root \\ default_root()) do
    with {:ok, events} <- inventory(root) do
      observed =
        events
        |> Enum.group_by(& &1.path)
        |> Map.new(fn {path, rows} ->
          {path, Enum.frequencies_by(rows, & &1.kind)}
        end)

      if observed == @expected do
        {:ok, %{status: :reviewed_static_inventory, files: map_size(@expected),
                recognized_writes: length(events), events: events,
                dynamic_bypass_excluded?: true, runtime_coverage_proven?: false}}
      else
        {:error, {:writer_inventory_changed, %{expected: @expected,
                                                observed: observed, events: events}}}
      end
    end
  end

  defp default_root do
    Path.expand("../../../../../", __DIR__)
  end

  defp event_kind({:|, _meta, [_map, fields]}) when is_list(fields) do
    if Keyword.keyword?(fields) and Keyword.has_key?(fields, :wm),
      do: :map_update, else: nil
  end

  defp event_kind({{:., _, [mod, function]}, _, args})
       when is_atom(function) and is_list(args) do
    path = alias_parts(mod)

    cond do
      path == [:Map] and function in
          [:put, :put_new, :replace, :replace!, :update, :update!] and
          wm_key_argument?(args) ->
        :map_put

      function == :upsert and path in
          [[:WorkingMemory], [:Brain, :WorkingMemory]] ->
        :working_memory_upsert

      true ->
        nil
    end
  end

  defp event_kind(_), do: nil

  # Direct Map.put(state, :wm, value), or piped state |> Map.put(:wm, value).
  defp wm_key_argument?([_map, :wm | _]), do: true
  defp wm_key_argument?([:wm | _]), do: true
  defp wm_key_argument?(_), do: false

  defp alias_parts({:__aliases__, _, parts}) when is_list(parts), do: parts
  defp alias_parts(_), do: []

  defp node_line({_, meta, _}) when is_list(meta), do: Keyword.get(meta, :line, 0)
  defp node_line(_), do: 0
end
