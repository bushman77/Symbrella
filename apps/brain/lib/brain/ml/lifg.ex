defmodule Brain.ML.LIFG do
  @moduledoc """
  Build explainability-oriented LIFG blocks for ML turn records.
  """

  @spec build_block(term(), boolean()) :: map()
  def build_block(lifg, hydrate_lex?) do
    {choices, token_meta} = extract_choices_and_tokens(lifg)

    winners =
      choices
      |> Enum.map(&normalize_choice(&1, token_meta))
      |> maybe_hydrate_lex(hydrate_lex?)

    %{
      last_update: lifg,
      winners: winners
    }
  end

  defp extract_choices_and_tokens(nil), do: {[], %{}}

  defp extract_choices_and_tokens(%{} = m) do
    last =
      mget(m, :last) ||
        mget_in(m, [:state, :last]) ||
        mget_in(m, [:lifg, :last]) ||
        mget_in(m, [:out, :last])

    tokens =
      (mget(m, :tokens) ||
         mget_in(m, [:state, :tokens]) ||
         (is_map(last) && (mget(last, :tokens) || mget(last, "tokens"))) ||
         [])
      |> List.wrap()
      |> Enum.filter(&is_map/1)

    token_meta =
      Enum.reduce(tokens, %{}, fn t, acc ->
        idx = mget(t, :index) || mget(t, :token_index)
        if is_integer(idx), do: Map.put(acc, idx, t), else: acc
      end)

    choices =
      (mget(m, :choices) ||
         mget(m, :winners) ||
         mget_in(m, [:out, :choices]) ||
         mget_in(m, [:lifg, :choices]) ||
         (is_map(last) && (mget(last, :choices) || mget(last, "choices"))) ||
         [])
      |> List.wrap()
      |> Enum.filter(&is_map/1)

    {choices, token_meta}
  end

  defp extract_choices_and_tokens(_), do: {[], %{}}

  defp normalize_choice(%{} = ch, token_meta) when is_map(token_meta) do
    id =
      ch[:chosen_id] ||
        ch["chosen_id"] ||
        ch[:id] ||
        ch["id"]

    tok_idx = ch[:token_index] || ch["token_index"]
    tok = if is_integer(tok_idx), do: Map.get(token_meta, tok_idx), else: nil

    {chosen_word, chosen_pos, chosen_sense} = parse_cell_id(id)

    alt_ids0 = ch[:alt_ids] || ch["alt_ids"] || []
    alt_ids = alt_ids0 |> List.wrap() |> Enum.take(12)

    %{
      token_index: tok_idx,
      phrase: tok && (mget(tok, :phrase) || mget(tok, :raw) || mget(tok, :text)),
      span: tok && (mget(tok, :span) || mget(tok, :range)),
      mw: tok && (mget(tok, :mw) || mget(tok, :multiword)),
      lemma: ch[:lemma] || ch["lemma"],
      chosen_id: id,
      chosen_word: chosen_word,
      chosen_pos: chosen_pos,
      chosen_sense: chosen_sense,
      margin: ch[:margin] || ch["margin"],
      score: ch[:score] || ch["score"] || ch[:prob] || ch["prob"],
      scores: ch[:scores] || ch["scores"],
      alt_ids: alt_ids
    }
  end

  defp parse_cell_id(id) when is_binary(id) do
    case String.split(id, "|", parts: 3) do
      [w, pos, sense] -> {w, pos, sense}
      [w, pos] -> {w, pos, nil}
      [w] -> {w, nil, nil}
      _ -> {nil, nil, nil}
    end
  end

  defp parse_cell_id(_), do: {nil, nil, nil}

  defp maybe_hydrate_lex(winners, false), do: winners

  defp maybe_hydrate_lex(winners, true) when is_list(winners) do
    Enum.map(winners, fn w ->
      id = w[:chosen_id]

      lex =
        if is_binary(id) and Code.ensure_loaded?(Db) and Code.ensure_loaded?(Db.BrainCell) do
          safe_fetch_cell_lex(id)
        else
          nil
        end

      if is_map(lex), do: Map.put(w, :lex, lex), else: w
    end)
  end

  defp safe_fetch_cell_lex(id) do
    case Db.get(Db.BrainCell, id) do
      nil ->
        nil

      row ->
        %{
          id: row.id,
          word: Map.get(row, :word),
          pos: Map.get(row, :pos),
          type: Map.get(row, :type),
          definition: Map.get(row, :definition),
          example: Map.get(row, :example),
          synonyms: Map.get(row, :synonyms),
          antonyms: Map.get(row, :antonyms)
        }
    end
  end

  defp mget(%{} = m, k), do: Map.get(m, k) || Map.get(m, to_string(k))
  defp mget(_, _), do: nil

  defp mget_in(m, [k | rest]) when is_map(m) do
    case mget(m, k) do
      %{} = next -> mget_in(next, rest)
      other -> other
    end
  end

  defp mget_in(_, _), do: nil
end
