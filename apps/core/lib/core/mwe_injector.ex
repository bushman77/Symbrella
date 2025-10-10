defmodule Core.MWE.Injector do
  @moduledoc ~S"""
  Inject multiword-expression (MWE) tokens by scanning word-level n-grams.

  - Accepts a list of single-word tokens (boundary-cleaned) with `span: {i, i+1}`.
  - Builds n-grams (default 2..4) and uses an `exists?/1` callback to decide which to inject.
  - Emits `%{phrase, mw: true, span: {i, j}, n: n, source: :mwe}` tokens.
  - Returns original + injected tokens, **deduped and ordered**: all single words first (by start),
    then MWEs (by start; shorter before longer at the same start).
  """

  @type token :: map()

  @spec inject([token], keyword()) :: [token]
  def inject(tokens, opts \\ []) when is_list(tokens) do
    len = length(tokens)
    max_n = Keyword.get(opts, :max_n, 4)
    exists? = Keyword.get(opts, :exists?, &__MODULE__.default_exists?/1)

    new_mwes =
      if len <= 0 do
        []
      else
        0..(len - 1)//1
        |> Enum.reduce([], fn i, acc ->
          t = Enum.at(tokens, i)
          p = tok_phrase(t)

          if single_word_string?(p) do
            max_here = min(max_n, len - i)

            # use explicit-step range; if max_here < 2 the range is empty
            Enum.reduce(2..max_here//1, acc, fn n, acc_in ->
              slice = Enum.slice(tokens, i, n)

              if Enum.all?(slice, fn tt -> single_word_string?(tok_phrase(tt)) end) do
                phrase2 =
                  slice
                  |> Enum.map(&tok_phrase/1)
                  |> Enum.filter(&is_binary/1)
                  |> Enum.join(" ")

                if exists?.(phrase2) do
                  [%{phrase: phrase2, mw: true, span: {i, i + n}, n: n, source: :mwe} | acc_in]
                else
                  acc_in
                end
              else
                acc_in
              end
            end)
          else
            acc
          end
        end)
        |> Enum.reverse()
      end

    tokens
    |> Kernel.++(new_mwes)
    |> Enum.uniq_by(fn t -> {tok_span(t), tok_phrase(t), tok_mw?(t)} end)
    # words first, then MWEs; within group: start asc; for MWEs at same start: shorter first
    |> Enum.sort_by(fn t -> {mw_key(t), start_index(t), length_key(t)} end)
  end

  @doc "Default repo check: inject nothing unless caller provides an exists?/1."
  @spec default_exists?(String.t()) :: boolean
  def default_exists?(_phrase), do: false

  # ──────────────────────────────────────────────────────────
  # SAFE ACCESS HELPERS (support maps and %Core.Token{}-like structs)
  # ──────────────────────────────────────────────────────────

  defp tok_phrase(%_struct{phrase: p}) when is_binary(p), do: p

  defp tok_phrase(%{} = t) do
    Map.get(t, :phrase) ||
      Map.get(t, "phrase") ||
      Map.get(t, :norm) ||
      Map.get(t, "norm") ||
      Map.get(t, :text) ||
      Map.get(t, "text")
  end

  defp tok_phrase(_), do: nil

  defp tok_span(%_struct{span: s}) when is_tuple(s), do: s
  defp tok_span(%{} = t), do: Map.get(t, :span) || Map.get(t, "span")
  defp tok_span(_), do: nil

  defp tok_mw?(%_struct{mw: v}) when is_boolean(v), do: v
  defp tok_mw?(%{} = t), do: Map.get(t, :mw) == true or Map.get(t, "mw") == true
  defp tok_mw?(_), do: false

  defp tok_n(%_struct{n: n}) when is_integer(n), do: n

  defp tok_n(%{} = t) do
    case Map.get(t, :n) || Map.get(t, "n") do
      i when is_integer(i) -> i
      _ -> nil
    end
  end

  defp tok_n(_), do: nil

  # ──────────────────────────────────────────────────────────
  # ORDERING HELPERS
  # ──────────────────────────────────────────────────────────

  defp mw_key(t), do: if(tok_mw?(t), do: 1, else: 0)

  defp start_index(t) do
    case tok_span(t) do
      {s, _e} when is_integer(s) -> s
      _ -> 9_000_000
    end
  end

  defp length_key(t) do
    if tok_mw?(t) do
      case tok_n(t) do
        n when is_integer(n) -> n
        _ -> 99_000
      end
    else
      0
    end
  end

  # ──────────────────────────────────────────────────────────
  # PREDICATES (no guards calling remote functions)
  # ──────────────────────────────────────────────────────────

  # True only for binaries with no spaces
  defp single_word_string?(p) when is_binary(p), do: not String.contains?(p, " ")
  defp single_word_string?(_), do: false
end
