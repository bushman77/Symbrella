defmodule Core.Math do
  @moduledoc """
  Core.Math — pure-Elixir math primitives for your semantics-first AI.

  Provides:
    • Exact rationals `{num, den}` (no floats)
    • Sequence → polynomial fitting via finite differences (Newton/binomial basis)
    • Convert to standard polynomial, evaluate, pretty-print
    • Quick identity/sequence checks (counterexample search)

  ## Quick start (IEx)

      iex> alias Core.Math, as: M

  Fit squares:
      iex> {:ok, {:binomial, d}} = M.fit_polynomial([0,1,4,9,16])
      iex> d
      [{0, 1}, {1, 1}, {2, 1}]
      iex> p = M.poly_from_binomial(d)
      iex> M.pretty_poly(p)
      "n^2"
      iex> M.eval_binomial(d, 5)
      25

  Verify n^3 - n:
      iex> seq = Enum.map(0..6, &(&1*&1*&1 - &1))
      iex> {:ok, {:binomial, d2}} = M.fit_polynomial(seq)
      iex> p2 = M.poly_from_binomial(d2)
      iex> M.verify_sequence(seq, {:poly, p2})
      :ok
  """

  @type rat :: {integer(), pos_integer()}
  # a0 + a1*n + a2*n^2 + ...
  @type poly :: [rat]
  # d0..dk for a(n)=Σ d_k*C(n,k)
  @type binomial_coeffs :: [rat]

  # -------- rationals --------

  @spec r(integer(), integer()) :: rat
  def r(num, den \\ 1)
  def r(_num, 0), do: raise(ArgumentError, "denominator cannot be 0")

  def r(num, den) when is_integer(num) and is_integer(den) do
    {n, d} = if den < 0, do: {-num, -den}, else: {num, den}
    g = Integer.gcd(abs(n), d)
    {div(n, g), div(d, g)}
  end

  @spec r_add(rat, rat) :: rat
  def r_add({a, b}, {c, d}), do: r(a * d + c * b, b * d)

  @spec r_sub(rat, rat) :: rat
  def r_sub(x, y), do: r_add(x, r_neg(y))

  @spec r_mul(rat, rat) :: rat
  def r_mul({a, b}, {c, d}), do: r(a * c, b * d)

  @spec r_div(rat, rat) :: rat
  def r_div(_x, {0, _}), do: raise(ArgumentError, "division by zero")
  def r_div({a, b}, {c, d}), do: r(a * d, b * c)

  @spec r_neg(rat) :: rat
  def r_neg({a, b}), do: {-a, b}

  @spec r_pow(rat, non_neg_integer()) :: rat
  def r_pow({_a, _b}, 0), do: {1, 1}

  def r_pow({a, b}, k) when is_integer(k) and k > 0 do
    r(Integer.pow(a, k), Integer.pow(b, k))
  end

  # -------- fitting & eval --------

  @spec fit_polynomial([integer()], pos_integer()) ::
          {:ok, {:binomial, binomial_coeffs}} | {:error, :not_polynomial}
  def fit_polynomial(seq, _max_degree \\ 16) when is_list(seq) and length(seq) >= 2 do
    diffs = difference_table(seq)
    d = for [x | _] <- diffs, do: r(x, 1)

    last_row = List.last(diffs)

    if Enum.all?(last_row, fn x -> x == 0 end) do
      {:ok, {:binomial, d}}
    else
      {:error, :not_polynomial}
    end
  end

  @spec eval_binomial(binomial_coeffs, integer()) :: integer() | rat
  def eval_binomial(d, n) when is_integer(n) and n >= 0 do
    d
    |> Enum.with_index()
    |> Enum.reduce({0, 1}, fn {dk, k}, acc ->
      r_add(acc, r_mul(dk, r(choose(n, k), 1)))
    end)
    |> to_int_or_rat()
  end

  @spec poly_from_binomial(binomial_coeffs) :: poly
  def poly_from_binomial(d) do
    Enum.with_index(d)
    |> Enum.reduce([], fn {dk, k}, poly ->
      term = poly_choose_k(k) |> poly_scale(dk) |> poly_div_int(fact(k))
      poly_add(poly, term)
    end)
  end

  @spec poly_eval(poly, integer()) :: integer() | rat
  def poly_eval(poly, n) when is_integer(n) do
    Enum.reverse(poly)
    |> Enum.reduce({0, 1}, fn coeff, acc -> r_add(coeff, r_mul(acc, r(n, 1))) end)
    |> to_int_or_rat()
  end

  @spec pretty_poly(poly) :: String.t()
  def pretty_poly(poly) do
    parts =
      poly
      |> Enum.with_index()
      |> Enum.reject(fn {c, _} -> c == {0, 1} end)
      |> Enum.map(&term_to_string/1)

    case parts do
      [] -> "0"
      [h | t] -> String.trim_leading(h, "+") <> Enum.join(t, "")
    end
  end

  @spec verify_sequence([integer()], {:binomial, binomial_coeffs} | {:poly, poly}) ::
          :ok | {:counterexample, non_neg_integer(), integer(), integer() | rat}
  def verify_sequence(seq, {:binomial, d}), do: do_verify(seq, &eval_binomial(d, &1))
  def verify_sequence(seq, {:poly, p}), do: do_verify(seq, &poly_eval(p, &1))

  @spec check_identity(
          {:binomial, binomial_coeffs} | {:poly, poly},
          {:binomial, binomial_coeffs} | {:poly, poly},
          Range.t()
        ) ::
          :likely_true | {:counterexample, integer(), integer() | rat, integer() | rat}
  def check_identity(left, right, range \\ 0..12) do
    Enum.reduce_while(range, :likely_true, fn n, _ ->
      lv = eval_any(left, n)
      rv = eval_any(right, n)
      if lv == rv, do: {:cont, :likely_true}, else: {:halt, {:counterexample, n, lv, rv}}
    end)
  end

  # -------- internals --------

  defp eval_any({:binomial, d}, n), do: eval_binomial(d, n)
  defp eval_any({:poly, p}, n), do: poly_eval(p, n)

  defp do_verify(seq, eval_fun) do
    seq
    |> Enum.with_index()
    |> Enum.reduce_while(:ok, fn {exp, n}, _ ->
      got = eval_fun.(n)
      if got == exp, do: {:cont, :ok}, else: {:halt, {:counterexample, n, exp, got}}
    end)
  end

  defp difference_table(seq), do: do_diff([seq], seq)
  defp do_diff(cols, [_]), do: cols

  defp do_diff(cols, list) do
    next = pairwise_diff(list)
    do_diff(cols ++ [next], next)
  end

  defp pairwise_diff(list) do
    list
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.map(fn [a, b] -> b - a end)
  end

  defp choose(_n, k) when k < 0, do: 0
  defp choose(n, 0) when n >= 0, do: 1
  defp choose(n, k) when k > n, do: 0

  defp choose(n, k) do
    k = min(k, n - k)
    Enum.reduce(1..k, 1, fn i, acc -> div(acc * (n - k + i), i) end)
  end

  defp fact(0), do: 1
  defp fact(n), do: Enum.reduce(1..n, 1, &*/2)

  defp poly_add(a, b) do
    {short, long} = if length(a) < length(b), do: {a, b}, else: {b, a}
    pad = short ++ List.duplicate({0, 1}, length(long) - length(short))
    Enum.zip(pad, long) |> Enum.map(fn {x, y} -> r_add(x, y) end)
  end

  defp poly_scale(poly, rat), do: Enum.map(poly, &r_mul(&1, rat))

  defp poly_div_int(poly, k) when is_integer(k) and k > 0,
    do: Enum.map(poly, fn {a, b} -> r(a, b * k) end)

  defp poly_choose_k(0), do: [{1, 1}]

  defp poly_choose_k(k) when k > 0 do
    Enum.reduce(0..(k - 1), [{1, 1}], fn i, poly ->
      # multiply by (n - i) => [-i, 1]
      mul(poly, [{-i, 1}, {1, 1}])
    end)
  end

  defp mul(a, b) do
    deg = length(a) - 1 + (length(b) - 1)
    base = List.duplicate({0, 1}, deg + 1)

    Enum.with_index(a)
    |> Enum.reduce(base, fn {ai, i}, acc ->
      Enum.with_index(b)
      |> Enum.reduce(acc, fn {bj, j}, acc2 ->
        List.update_at(acc2, i + j, fn curr -> r_add(curr, r_mul(ai, bj)) end)
      end)
    end)
  end

  defp to_int_or_rat({num, den}), do: if(den == 1, do: num, else: {num, den})

  defp term_to_string({{num, den}, 0}) do
    if den == 1, do: Integer.to_string(num), else: "#{num}/#{den}"
  end

  defp term_to_string({{num, den}, 1}) do
    coef =
      cond do
        {num, den} == {1, 1} -> ""
        {num, den} == {-1, 1} -> "-"
        den == 1 -> Integer.to_string(abs(num))
        true -> "#{abs(num)}/#{den}"
      end

    sign = if num < 0, do: "-", else: "+"
    sign <> coef <> "n"
  end

  defp term_to_string({{num, den}, k}) do
    coef =
      cond do
        {num, den} == {1, 1} -> ""
        {num, den} == {-1, 1} -> "-"
        den == 1 -> Integer.to_string(abs(num))
        true -> "#{abs(num)}/#{den}"
      end

    sign = if num < 0, do: "-", else: "+"
    sign <> coef <> "n^" <> Integer.to_string(k)
  end
end
