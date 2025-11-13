# lib/core/intent/sliding.ex
defmodule Core.Intent.Sliding do
  @moduledoc "Crude sliding-window intent + per-token bias for LIFG."

  @wh ~w(what when where why how which who whom whose)
  @aux ~w(do does did is are am was were have has had can could will would shall should)
  @modal ~w(can could will would shall should may might must)
  @pron ~w(i you he she it we they me him her us them my your our their mine yours ours theirs)
  @det ~w(a an the this that these those my your our their)
  @prep ~w(of to in for on with at from into by about like through after over between out against during without before under around among)
  @interj ~w(hi hello hey yo sup thanks thank thx ty)
  @func Enum.into(@aux ++ @modal ++ @pron ++ @det ++ @prep, MapSet.new())

  # ——— public entrypoint ———
  def classify_and_bias(si) when is_map(si) do
    toks = si.tokens || []
    {seq, forms} = coarse_pos_sequence(toks)

    intent =
      cond do
        questionish?(forms) or
            match(seq, [[:adverb, :aux, :pron], [:aux, :pron], [:modal, :pron, :verb]]) ->
          :question

        starts_with?(forms, @interj) ->
          :greet

        true ->
          :inform
      end

    bias = build_bias(intent, seq, forms)

    si
    |> Map.put(:intent, intent)
    |> Map.put(:source, :sliding)
    |> Map.put(:intent_bias, bias)
    |> Map.put(:confidence, confidence(intent, seq))
    |> Map.put(:keyword, extract_keyword(intent, forms))
  end

  # ——— sliding windows over coarse POS ———
  defp coarse_pos_sequence(tokens) do
    forms =
      Enum.map(tokens, fn t ->
        raw = Map.get(t, :phrase, "") |> String.trim()
        low = String.downcase(raw)
        {raw, low}
      end)

    seq =
      Enum.map(forms, fn {raw, low} ->
        cond do
          low in @interj -> :interj
          low in @wh -> :wh
          low in @aux -> :aux
          low in @modal -> :modal
          low in @pron -> :pron
          low in @det -> :det
          low in @prep -> :prep
          capitalized?(raw) and String.length(raw) >= 2 -> :proper
          String.ends_with?(low, "?") -> :qmark
          true -> guess_open_class(low)
        end
      end)

    {seq, forms}
  end

  defp guess_open_class(low) do
    cond do
      low in ~w(stream play launch open install configure fix run) ->
        :verb

      low in ~w(phone pc client game app settings sunshine moonlight parsec steam everquest) ->
        :noun

      true ->
        :unknown
    end
  end

  defp capitalized?(<<c::utf8, _rest::binary>>), do: c >= ?A and c <= ?Z
  defp capitalized?(_), do: false

  defp match(seq, patterns) do
    Enum.any?(patterns, fn pat ->
      Enum.chunk_every(seq, length(pat), 1, :discard) |> Enum.any?(&(&1 == pat))
    end)
  end

  defp starts_with?([{_raw, low} | _], lex), do: low in lex
  defp starts_with?(_forms, _lex), do: false

  defp questionish?(forms) do
    last_low =
      case List.last(forms) do
        {_, l} -> l
        _ -> ""
      end

    starts_with?(forms, @wh) or String.ends_with?(last_low, "?")
  end

  # ——— bias builder (keys: token_index) ———
  # Uses the already-computed seq so types/labels are consistent.
  defp build_bias(:question, seq, forms) do
    seq
    |> Enum.with_index()
    |> Enum.map(fn {pos, ix} ->
      low = elem(Enum.at(forms, ix), 1)

      v =
        cond do
          pos in [:verb, :proper, :noun] -> 0.25
          low in @func -> -0.18
          pos == :unknown -> 0.0
          true -> -0.05
        end

      {ix, v}
    end)
    |> Map.new()
  end

  defp build_bias(:greet, _seq, forms) do
    forms
    |> Enum.with_index()
    |> Enum.map(fn {{_raw, low}, ix} ->
      v = if low in @interj, do: 0.3, else: -0.05
      {ix, v}
    end)
    |> Map.new()
  end

  defp build_bias(_other, seq, _forms) do
    seq
    |> Enum.with_index()
    |> Enum.map(fn {pos, ix} ->
      v = if pos in [:proper, :noun, :verb], do: 0.12, else: -0.05
      {ix, v}
    end)
    |> Map.new()
  end

  defp confidence(:question, _seq), do: 0.7
  defp confidence(:greet, _seq), do: 0.7
  defp confidence(_other, _seq), do: 0.6

  defp extract_keyword(:question, forms) do
    case forms do
      [{_, l} | _] when l in @wh -> l
      _ -> nil
    end
  end

  defp extract_keyword(:greet, forms) do
    case forms do
      [{_, l} | _] when l in @interj -> l
      _ -> nil
    end
  end

  defp extract_keyword(_, _), do: nil
end
