defmodule Core.Vectors do
  @moduledoc """
  Thin facade around your embedding source.

  Replace `fetch/1` with a real lookup (DB, ETS, Nx, file, HTTP, etc.).
  For now: stable, deterministic 3-dim vectors from a key (word/tuple/id).
  """

  @type key :: term()
  @type vec :: [number()]

  @spec fetch(key()) :: vec() | nil
  def fetch(key) do
    # DEMO: stable 3-D unit vector from the key (phash2). Swap with real storage.
    h = :erlang.phash2(key, 1_000_003)
    a = (rem(h, 997) / 996) * :math.pi() * 2
    b = (rem(div(h, 997), 991) / 990) * :math.pi() * 2

    # Spherical-ish mapping, normalized
    x = :math.cos(a)
    y = :math.sin(a) * :math.cos(b)
    z = :math.sin(b)

    norm = :math.sqrt(x * x + y * y + z * z) |> max(1.0e-9)
    [x / norm, y / norm, z / norm]
  end
end

