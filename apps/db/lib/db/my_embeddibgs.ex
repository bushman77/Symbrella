defmodule MyEmbeddings do
  @dim Application.compile_env(:db, :embedding_dim, 1536)

  def embed(text) when is_binary(text) do
    # your real implementation (or the deterministic test one)
    vec = for i <- 1..@dim, do: :math.sin(byte_size(text) + i)
    {:ok, vec}
  end

  def embed!(text) when is_binary(text) do
    case embed(text) do
      {:ok, vec} -> vec
      other -> raise "Embedding failed: #{inspect(other)}"
    end
  end
end
