# Usage: mix run scripts/probe_lifg_confidence.exs "hey symbrella what are you up to?"
#
# Read-only diagnostic for LIFG Stage1 confidence. Per token prints the chosen
# sense, top_p, slate size, and the spread of final logits across the
# definition_context_evidence rows (identical logits => uniform softmax).
# Note: runs the full prod pipeline, so the turn may land in STM.

sentence =
  case System.argv() do
    [] -> "hey symbrella what are you up to?"
    args -> Enum.join(args, " ")
  end

get = fn m, k ->
  cond do
    is_map(m) and Map.has_key?(m, k) -> Map.get(m, k)
    is_map(m) -> Map.get(m, to_string(k))
    true -> nil
  end
end

r = fn
  x when is_number(x) -> Float.round(x * 1.0, 4)
  x -> x
end

IO.puts("""

lifg_sense_rank_decay    = #{inspect(Application.get_env(:brain, :lifg_sense_rank_decay, 0.0))}
lifg_softmax_temperature = #{inspect(Application.get_env(:brain, :lifg_softmax_temperature, 1.0))}
Sentence: #{sentence}
""")

si = Core.resolve_input(sentence, mode: :prod)

for ch <- List.wrap(get.(si, :lifg_choices)) do
  focus = get.(ch, :sense_focus) || %{}
  rows = List.wrap(get.(ch, :definition_context_evidence))
  logits = rows |> Enum.map(&get.(&1, :final_logit)) |> Enum.filter(&is_number/1)

  spread =
    case logits do
      [] -> "n/a"
      _ -> "min=#{r.(Enum.min(logits))} max=#{r.(Enum.max(logits))} distinct=#{logits |> Enum.map(&r.(&1)) |> Enum.uniq() |> length()}/#{length(logits)}"
    end

  top_rows =
    rows
    |> Enum.sort_by(&(-(get.(&1, :final_logit) || 0)))
    |> Enum.take(4)
    |> Enum.map(&"#{get.(&1, :id)}=#{r.(get.(&1, :final_logit))}")
    |> Enum.join("  ")

  IO.puts("""
  token #{get.(ch, :token_index)}  chosen=#{get.(ch, :id)}
    top_p=#{r.(get.(ch, :score))}  slate=#{inspect(get.(focus, :candidates))}  alt_ids=#{inspect(get.(ch, :alt_ids))}
    logits: #{spread}
    top: #{top_rows}
  """)
end

summary = get.(si, :comprehension) || %{}
IO.puts("stats:      #{inspect(get.(summary, :stats))}")
IO.puts("uncertain:  #{inspect(get.(summary, :uncertain))}")
IO.puts("understood: #{inspect(get.(summary, :understood))}")
