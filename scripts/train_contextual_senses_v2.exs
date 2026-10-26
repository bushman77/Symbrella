# Symbrella / development — offline contextual sense learning pilot.
# Place this file at ~/Symbrella/scripts/train_contextual_senses.exs
# From the umbrella root:
#   mix run --no-start scripts/train_contextual_senses.exs
#   mix run --no-start scripts/train_contextual_senses.exs --steps 320 --negative-mode same_norm
#   mix run --no-start scripts/train_contextual_senses.exs --steps 320 --negative-mode all_batch
# Two modes share the same grouped minibatches, RNG seed, and dataset.
# same_norm masks unrelated words out of the contrastive denominator.
#
# NO database writes, no model persistence, no folders, no live Brain activity.
# The current native vocabulary must already be populated.
# Dictionary examples are weak supervision, not manually reviewed gold labels.
# Split is by normalized word; "there" and named comparison words NEVER train.
# Evaluation senses with near-equivalent definitions may be falsely labeled
# as different, so pilot accuracy is only a weak-label diagnostic.
# Raw brain_cells.numeric_id is printed as an identity address, NOT an input feature.

{:ok, _} = Application.ensure_all_started(:db)
case Db.start_link() do
  {:ok, _pid} -> :ok
  {:error, {:already_started, _pid}} -> :ok
  other -> raise "Cannot start Db repository: #{inspect(other)}"
end

defmodule Symbrella.ContextualSensePilot do
  @moduledoc false
  import Ecto.Query
  import Nx.Defn
  alias Db.{BrainCell, NativeVocab, NativeVocabToken}

  @reserved_norms ~w(there bank bat draw light)
  @pos ~w(noun verb adjective adverb pronoun determiner preposition conjunction interjection)
  @ignore MapSet.new(~w(a an and are as at be been but by did do does for from had has have
    he her him his i in into is it its me my not of on or our she that the their them
    there these they this those to us was we were what when where which who will with
    would you your))

  # Shared mean-pooled token encoder. No numeric addresses or sense ID strings used.
  defn encode(table, input_ids, masks) do
    {n, width} = Nx.shape(input_ids)
    vectors = Nx.take(table, input_ids, axis: 0)
    m = masks |> Nx.as_type({:f, 32}) |> Nx.reshape({n, width, 1})
    sum = Nx.multiply(vectors, m) |> Nx.sum(axes: [1])
    count = Nx.sum(m, axes: [1]) |> Nx.max(Nx.broadcast(1.0, {n, 1}))
    mean = Nx.divide(sum, count)
    magnitude = mean |> Nx.multiply(mean) |> Nx.sum(axes: [1]) |>
      Nx.add(1.0e-12) |> Nx.sqrt() |> Nx.reshape({n, 1})
    Nx.divide(mean, magnitude)
  end

  # Candidate mask defines which glosses are genuine alternatives for each row.
  # same_norm: only senses of the same normalized word compete.
  # all_batch: the original objective for a controlled comparison.
  defn loss(table, contexts, context_masks, glosses, gloss_masks, positives, candidate_mask, temp) do
    left = encode(table, contexts, context_masks)
    right = encode(table, glosses, gloss_masks)
    raw_logits = Nx.dot(left, Nx.transpose(right)) |> Nx.divide(temp)
    logits = Nx.select(Nx.greater(candidate_mask, 0.0), raw_logits, Nx.broadcast(-10_000.0, Nx.shape(raw_logits)))
    max_logits = Nx.reduce_max(logits, axes: [1], keep_axes: true)
    log_denom = logits |> Nx.subtract(max_logits) |> Nx.exp() |>
      Nx.sum(axes: [1], keep_axes: true) |> Nx.log() |> Nx.add(max_logits)
    positive_logits = Nx.multiply(raw_logits, positives) |> Nx.sum(axes: [1]) |>
      Nx.reshape({:auto, 1})
    Nx.subtract(log_denom, positive_logits) |> Nx.mean()
  end

  defn step(table, contexts, context_masks, glosses, gloss_masks, positives, candidate_mask, temp, lr) do
    {cost, gradient} = value_and_grad(table, fn weights ->
      loss(weights, contexts, context_masks, glosses, gloss_masks, positives, candidate_mask, temp)
    end)
    {Nx.subtract(table, Nx.multiply(gradient, lr)), cost}
  end

  def clean(value) when is_binary(value), do: value |> String.replace(~r/\s+/u, " ") |> String.trim()
  def clean(_), do: ""

  def remove_target(text, norm) do
    # Pilot restricts norms to single ASCII alphabetic words.
    text
    |> clean()
    |> String.replace(~r/\b#{Regex.escape(norm)}\b/ui, " ")
    |> clean()
  end

  def usable?(%{norm: norm, definition: definition, example: example}) do
    is_binary(norm) and Regex.match?(~r/^[a-z]{3,18}$/, norm) and
      byte_size(clean(definition)) >= 12 and byte_size(clean(example)) >= 15 and
      String.match?(String.downcase(example), ~r/\b#{Regex.escape(norm)}\b/u)
  end

  def record(cell) do
    %{
      id: cell.id, numeric_id: cell.numeric_id, norm: cell.norm, pos: cell.pos,
      context: remove_target(cell.example, cell.norm),
      gloss: remove_target(cell.definition, cell.norm)
    }
  end

  def eligible_records(cells) do
    cells
    |> Enum.filter(&usable?/1)
    |> Enum.map(&record/1)
    |> Enum.reject(&(byte_size(&1.context) < 8 or byte_size(&1.gloss) < 8))
    |> Enum.uniq_by(&{&1.norm, String.downcase(&1.context), String.downcase(&1.gloss)})
  end

  def fetch_cells(seed_norms, max_senses) do
    # The BrainCell :status is a runtime selection gate, not a dictionary-quality
    # indicator. The original pilot accidentally required "active" here and
    # rejected the local dictionary sample before any training could run.
    # Sample addresses deterministically, but never use their values as features.
    candidate_query =
      from c in BrainCell,
        where: not is_nil(c.numeric_id) and c.pos in ^@pos and
          not is_nil(c.definition) and not is_nil(c.example) and
          fragment("mod(?, 97) = 0", c.numeric_id),
        order_by: [asc: c.numeric_id],
        limit: ^(seed_norms * 12),
        select: c.norm

    normalize_candidates = fn values ->
      values
      |> Enum.filter(&(is_binary(&1) and Regex.match?(~r/^[a-z]{3,18}$/, &1)))
      |> Enum.reject(&(&1 in @reserved_norms))
      |> Enum.uniq()
      |> Enum.take(seed_norms)
    end

    candidate_norms = candidate_query |> Db.all() |> normalize_candidates.()

    if candidate_norms == [] do
      raise "No candidate norms after dictionary filtering; check brain_cells " <>
              "definitions/examples and the numeric_id sample"
    end

    all_norms = Enum.uniq(candidate_norms ++ @reserved_norms)

    cells =
      Db.all(
        from c in BrainCell,
          where: c.norm in ^all_norms and c.pos in ^@pos and
            not is_nil(c.definition) and not is_nil(c.example),
          order_by: [asc: c.norm, asc: c.numeric_id],
          select: %{
            id: c.id, numeric_id: c.numeric_id, norm: c.norm, pos: c.pos,
            definition: c.definition, example: c.example
          }
      )

    # Test evidence suitability BEFORE capping senses. Otherwise eight early
    # unusable senses could hide usable senses later in the same norm.
    usable = eligible_records(cells)
    records =
      usable
      |> Enum.group_by(& &1.norm)
      |> Enum.sort_by(fn {norm, _group} -> norm end)
      |> Enum.flat_map(fn {_norm, group} -> Enum.take(group, max_senses) end)

    IO.inspect(%{
      sampled_norms: length(candidate_norms),
      fetched_rows: length(cells),
      usable_rows: length(usable),
      retained_rows: length(records),
      retained_norms: records |> Enum.map(& &1.norm) |> Enum.uniq() |> length()
    }, label: "CORPUS SELECTION AUDIT")

    if records == [], do: raise("No dictionary rows meet the example/gloss quality checks")
    records
  end

  def validation_norm?(norm) do
    norm in @reserved_norms or
      (:crypto.hash(:sha256, norm) |> :binary.decode_unsigned() |> rem(5)) == 0
  end

  def vocab do
    tokens = Db.all(from v in NativeVocabToken, select: {v.token, v.token_id})
    map = Map.new(tokens)
    Enum.each(~w([PAD] [UNK] [BOS] [EOS]), &Map.fetch!(map, &1))
    max_id = tokens |> Enum.map(&elem(&1, 1)) |> Enum.max()
    if max_id > 250_000, do: raise("Native vocabulary ID space too large for this pilot")
    {map, max_id + 1}
  end

  def token_ids(text, vocab, max_tokens) do
    parts = NativeVocab.tokenize(text)
    ids = parts |> Enum.take(max_tokens - 2) |> Enum.map(&Map.get(vocab, &1, vocab["[UNK]"]))
    [vocab["[BOS]"] | ids] ++ [vocab["[EOS]"]]
  end

  def encode_batch(texts, vocab, width) do
    ids = Enum.map(texts, &token_ids(&1, vocab, width))
    pads = vocab["[PAD]"]
    padded = Enum.map(ids, &(&1 ++ List.duplicate(pads, width - length(&1))))
    masks = Enum.map(ids, fn row ->
      List.duplicate(1, length(row)) ++ List.duplicate(0, width - length(row))
    end)
    {Nx.tensor(padded, type: {:s, 64}), Nx.tensor(masks, type: {:f, 32})}
  end

  def step_batch(table, records, vocab, width, lr, negative_mode) do
    n = length(records)
    {left, left_m} = encode_batch(Enum.map(records, & &1.context), vocab, width)
    {right, right_m} = encode_batch(Enum.map(records, & &1.gloss), vocab, width)
    positive_rows =
      for i <- 0..(n - 1) do
        for j <- 0..(n - 1), do: if(i == j, do: 1.0, else: 0.0)
      end
    positive = Nx.tensor(positive_rows, type: {:f, 32})
    norms = Enum.map(records, & &1.norm)
    candidate_rows =
      for i <- 0..(n - 1) do
        for j <- 0..(n - 1) do
          if negative_mode == "all_batch" or Enum.at(norms, i) == Enum.at(norms, j), do: 1.0, else: 0.0
        end
      end
    candidate_mask = Nx.tensor(candidate_rows, type: {:f, 32})
    {table, cost} = step(table, left, left_m, right, right_m,
      positive, candidate_mask, Nx.tensor(0.07, type: {:f, 32}), Nx.tensor(lr, type: {:f, 32}))
    {table, Nx.to_number(cost)}
  end

  # Keep complete sense groups in each minibatch. Never split a word's
  # senses across batches: doing so would silently discard hard negatives.
  # A fresh group shuffle each epoch keeps training order deterministic for
  # a fixed initial seed, while making all eligible senses eventually visible.
  def grouped_batches(train, batch_size) do
    groups = train |> Enum.group_by(& &1.norm) |>
      Enum.filter(fn {_norm, items} -> length(items) >= 2 end) |>
      Enum.sort_by(&elem(&1, 0)) |>
      Enum.map(&elem(&1, 1)) |> Enum.shuffle()

    {current, completed} = Enum.reduce(groups, {[], []}, fn group, {current, completed} ->
      if length(current) + length(group) > batch_size do
        {group, if(current == [], do: completed, else: [current | completed])}
      else
        {current ++ group, completed}
      end
    end)

    (if current == [], do: completed, else: [current | completed]) |> Enum.reverse()
  end

  def training_batches(train, batch_size, steps) do
    first = grouped_batches(train, batch_size)
    if first == [], do: raise("No training words have 2 or more eligible senses")
    # Generate enough full, independently shuffled epochs to reach steps.
    passes = div(steps + length(first) - 1, length(first)) + 1
    all = [first | Enum.map(2..passes, fn _ -> grouped_batches(train, batch_size) end)] |> Enum.flat_map(& &1)
    {Enum.take(all, steps), length(first)}
  end

  def overlap(context, gloss) do
    a = context |> NativeVocab.tokenize() |> Enum.reject(&MapSet.member?(@ignore, &1)) |> MapSet.new()
    b = gloss |> NativeVocab.tokenize() |> Enum.reject(&MapSet.member?(@ignore, &1)) |> MapSet.new()
    MapSet.intersection(a, b) |> MapSet.size()
  end

  defp score_group(table, group, vocab, width) do
    {context_ids, context_masks} = encode_batch(Enum.map(group, & &1.context), vocab, width)
    {gloss_ids, gloss_masks} = encode_batch(Enum.map(group, & &1.gloss), vocab, width)
    contexts = encode(table, context_ids, context_masks)
    glosses = encode(table, gloss_ids, gloss_masks)
    Nx.dot(contexts, Nx.transpose(glosses)) |> Nx.to_list()
  end

  def evaluate(table, records, vocab, width, verbose? \\ false) do
    groups = records |> Enum.group_by(& &1.norm) |> Enum.filter(fn {_norm, group} -> length(group) >= 2 end)
    if groups == [], do: raise("No multi-sense evaluation groups; increase --seed-norms")

    outcomes =
      Enum.flat_map(groups, fn {norm, group} ->
        scores = score_group(table, group, vocab, width)
        Enum.with_index(group)
        |> Enum.map(fn {item, i} ->
          row_scores = Enum.at(scores, i)
          predicted_j = row_scores |> Enum.with_index() |> Enum.max_by(fn {score, j} -> {score, -j} end) |> elem(1)
          lexical_j = group |> Enum.with_index() |>
            Enum.max_by(fn {candidate, j} -> {overlap(item.context, candidate.gloss), -j} end) |> elem(1)
          expected_score = Enum.at(row_scores, i)
          rank = 1 + Enum.count(row_scores, &(&1 > expected_score))
          result = %{
            norm: norm, expected: item.id, numeric_id: item.numeric_id,
            predicted: Enum.at(group, predicted_j).id, rank: rank,
            top1?: predicted_j == i, overlap_top1?: lexical_j == i,
            candidates: length(group)
          }
          if verbose? and i < 2, do: IO.inspect(result, label: "heldout_example")
          result
        end)
      end)

    count = length(outcomes)
    %{
      words: length(groups), cases: count,
      neural_top1: Enum.count(outcomes, & &1.top1?),
      neural_top1_pct: Float.round(100 * Enum.count(outcomes, & &1.top1?) / count, 2),
      overlap_top1: Enum.count(outcomes, & &1.overlap_top1?),
      overlap_top1_pct: Float.round(100 * Enum.count(outcomes, & &1.overlap_top1?) / count, 2),
      neural_top5: Enum.count(outcomes, &(&1.rank <= 5)),
      mean_reciprocal_rank: Float.round(Enum.sum(Enum.map(outcomes, &(1.0 / &1.rank))) / count, 4),
      sample_errors: outcomes |> Enum.reject(& &1.top1?) |> Enum.take(10),
      chance_top1_pct: Float.round(100 * Enum.sum(Enum.map(groups, fn {_n, _group} -> 1.0 end)) /
        count, 2)
    }
  end

  def pilot_seven(table, vocab, width, path) do
    if not File.regular?(path) do
      IO.puts("Seven-sentence pilot skipped; missing #{path}")
      :skipped
    else
      all_there = Db.all(from c in BrainCell,
        where: c.norm == "there" and not is_nil(c.definition),
        order_by: [asc: c.numeric_id],
        select: %{id: c.id, numeric_id: c.numeric_id, definition: c.definition}
      )
      glosses = Enum.map(all_there, &remove_target(&1.definition, "there"))
      {gloss_ids, gloss_masks} = encode_batch(glosses, vocab, width)
      all_vectors = encode(table, gloss_ids, gloss_masks)
      cases = path |> File.stream!() |> Enum.reject(&(String.trim(&1) == "")) |> Enum.map(&Jason.decode!/1)
      results = Enum.map(cases, fn item ->
        sentence = item["sentence"]
        {ids, masks} = encode_batch([remove_target(sentence, "there")], vocab, width)
        query = encode(table, ids, masks)
        scores = Nx.dot(query, Nx.transpose(all_vectors)) |> Nx.to_flat_list()
        ordered = Enum.zip(all_there, scores) |> Enum.sort_by(fn {_c, s} -> -s end)
        expected = item["expected_id"]
        rank = Enum.find_index(ordered, fn {c, _} -> c.id == expected end)
        outcome = %{
          expected: expected, predicted: hd(ordered) |> elem(0) |> Map.fetch!(:id),
          expected_rank: if(is_integer(rank), do: rank + 1, else: nil),
          top_three: Enum.take(Enum.map(ordered, fn {c, s} -> {c.id, Float.round(s, 4)} end), 3)
        }
        IO.inspect(outcome, label: "there_sentence")
        outcome
      end)
      total = length(results)
      top1 = Enum.count(results, &(&1.expected == &1.predicted))
      result = %{cases: total, correct: top1, top1_pct: Float.round(100 * top1 / max(total, 1), 2), comparison_lifg_correct: 3}
      IO.inspect(result, label: "THERE SEVEN-SENTENCE RESULT")
      result
    end
  end

  def coverage(records, vocab) do
    tokens = Enum.flat_map(records, fn r -> [r.context, r.gloss] end) |>
      Enum.flat_map(&NativeVocab.tokenize/1)
    n = length(tokens)
    unknown = Enum.count(tokens, &(not Map.has_key?(vocab, &1)))
    %{tokens: n, unknown: unknown, recognized_pct: Float.round(100 * (n - unknown) / max(n, 1), 2)}
  end

  def execute(options) do
    seed_norms = Keyword.fetch!(options, :seed_norms)
    max_senses = Keyword.fetch!(options, :max_senses)
    steps = Keyword.fetch!(options, :steps)
    batch_size = Keyword.fetch!(options, :batch)
    width = Keyword.fetch!(options, :max_tokens)
    dim = Keyword.fetch!(options, :dim)
    lr = Keyword.fetch!(options, :learning_rate)
    negative_mode = Keyword.fetch!(options, :negative_mode)

    {vocab, table_rows} = vocab()
    records = fetch_cells(seed_norms, max_senses)
    {train_unclean, eval} = Enum.split_with(records, &(not validation_norm?(&1.norm)))

    # Also block exact text duplicates across words and across the split.
    eval_contexts = eval |> Enum.map(&(String.downcase(&1.context))) |> MapSet.new()
    eval_glosses = eval |> Enum.map(&(String.downcase(&1.gloss))) |> MapSet.new()
    train = Enum.reject(train_unclean, fn row ->
      MapSet.member?(eval_contexts, String.downcase(row.context)) or
        MapSet.member?(eval_glosses, String.downcase(row.gloss))
    end)
    leaked_text_rows_removed = length(train_unclean) - length(train)
    if length(train) < batch_size * 2, do: raise("Only #{length(train)} training rows; increase --seed-norms")

    # Fixed seed controls both initialization and batch ordering.
    :rand.seed(:exsss, {11, 22, 33})
    initial = for _ <- 1..(table_rows * dim), do: (:rand.uniform() - 0.5) * 0.04
    table = initial |> Nx.tensor(type: {:f, 32}) |> Nx.reshape({table_rows, dim})
    {batches, batches_per_epoch} = training_batches(train, batch_size, steps)
    unique_training_ids_seen = batches |> List.flatten() |> Enum.map(& &1.id) |> Enum.uniq() |> length()

    IO.inspect(%{
      training_rows: length(train), training_norms: Enum.uniq_by(train, & &1.norm) |> length(),
      eval_rows: length(eval), eval_norms: Enum.uniq_by(eval, & &1.norm) |> length(),
      heldout_words: @reserved_norms, sampled_records: length(records),
      shared_text_rows_excluded_from_training: leaked_text_rows_removed,
      train_coverage: coverage(train, vocab), eval_coverage: coverage(eval, vocab),
      vocab_entries: map_size(vocab), table_rows: table_rows,
      embedding_dim: dim, batch_size: batch_size, steps: steps,
      negative_mode: negative_mode, training_batches_per_epoch: batches_per_epoch,
      unique_training_senses_seen: unique_training_ids_seen,
      training_sense_exposures: Enum.sum(Enum.map(batches, &length/1)),
      no_db_writes?: true, numeric_ids_used_as_features?: false
    }, label: "CONTEXTUAL DATASET", pretty: true)

    baseline = evaluate(table, eval, vocab, width)
    IO.inspect(baseline, label: "BEFORE TRAINING (random encoder)")
    IO.puts("\n=== Seven there sentences: BEFORE TRAINING ===")
    pilot_seven(table, vocab, width, "priv/there_sense_pilot.jsonl")

    {trained, _} = Enum.reduce(Enum.with_index(batches, 1), {table, 0.0}, fn {batch, step_no}, {weights, _} ->
      {weights, cost} = step_batch(weights, batch, vocab, width, lr, negative_mode)
      if rem(step_no, max(1, div(steps, 10))) == 0 or step_no == 1,
        do: IO.puts("step=#{step_no}/#{steps} loss=#{Float.round(cost, 5)}")
      {weights, cost}
    end)

    after_training = evaluate(trained, eval, vocab, width)
    IO.inspect(after_training, label: "HELDOUT WORD RESULT")
    IO.puts("\n=== Seven there sentences: AFTER TRAINING ===")
    pilot_seven(trained, vocab, width, "priv/there_sense_pilot.jsonl")
    IO.puts("\nAll training was in memory. No DB writes, saved weights, or live integration.")
    IO.puts("Caveat: dictionary-supplied example/gloss matches are weak labels; heldout words avoid same-word supervision leakage.")
  end
end

{opts, trailing, invalid} = OptionParser.parse(System.argv(),
  strict: [seed_norms: :integer, max_senses: :integer, steps: :integer, batch: :integer,
           max_tokens: :integer, dim: :integer, learning_rate: :float, negative_mode: :string]
)
if invalid != [] or trailing != [], do: raise("Invalid arguments: #{inspect({invalid, trailing})}")
options = [
  seed_norms: Keyword.get(opts, :seed_norms, 500),
  max_senses: Keyword.get(opts, :max_senses, 8),
  steps: Keyword.get(opts, :steps, 320),
  batch: Keyword.get(opts, :batch, 24),
  max_tokens: Keyword.get(opts, :max_tokens, 64),
  dim: Keyword.get(opts, :dim, 32),
  learning_rate: Keyword.get(opts, :learning_rate, 0.15),
  negative_mode: Keyword.get(opts, :negative_mode, "same_norm")
]
if not (options[:seed_norms] in 50..3000 and options[:max_senses] in 2..32 and
        options[:steps] in 1..2000 and options[:batch] in 4..64 and
        options[:max_senses] <= options[:batch] and
        options[:max_tokens] in 16..256 and options[:dim] in 8..128 and
        options[:learning_rate] > 0.0 and options[:learning_rate] <= 1.0 and
        options[:negative_mode] in ["same_norm", "all_batch"]),
  do: raise("Pilot arguments outside safe ranges: #{inspect(options)}")
Symbrella.ContextualSensePilot.execute(options)
