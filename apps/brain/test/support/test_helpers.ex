# apps/brain/test/support/test_helpers.ex
defmodule Brain.TestHelpers do
  @moduledoc """
  Test utilities for the Brain app — especially for async operations like WM gating.

  Helps avoid race conditions where OTP processes (e.g., BasalGanglia, WorkingMemory)
  update state *after* a synchronous function call returns.
  """

  @doc """
  Polls the current working memory for an item with a given ID.

  Returns `{:ok, item}` if found within `timeout_ms`, or `{:timeout, attempts}`
  if not found.

  ## Example

      iex> Brain.TestHelpers.wait_for_wm_item("test-id", 200)
      {:ok, %{id: "test-id", source: :lifg}}

  """
  @spec wait_for_wm_item(term(), pos_integer()) :: {:ok, map()} | {:timeout, non_neg_integer()}
  def wait_for_wm_item(id, timeout_ms) when is_integer(timeout_ms) and timeout_ms > 0 do
    deadline = System.monotonic_time(:millisecond) + timeout_ms
    poll_wm_item(id, deadline, 0)
  end

  @doc """
  Like `wait_for_wm_item/2`, but raises `ExUnit.AssertionError` on timeout.

  This is convenient for tests where the item *must* appear.

  ## Example

      iex> Brain.TestHelpers.assert_wm_item_exists("test-id", 200)
      %{
        id: "test-id",
        source: :lifg,
        score: 0.9
      }

  """
  def assert_wm_item_exists(id, timeout_ms \\ 200) do
    case wait_for_wm_item(id, timeout_ms) do
      {:ok, item} ->
        item

      {:timeout, attempts} ->
        raise ExUnit.AssertionError,
              "Timed out waiting for WM item with id: #{inspect(id)}. " <>
                "WM was empty or did not contain the expected item after #{attempts} attempts."
    end
  end

  defp poll_wm_item(id, deadline, attempts) do
    %{wm: wm} = Brain.snapshot_wm()

    case Enum.find(wm, &(Map.get(&1, :id) == id)) do
      nil ->
        if System.monotonic_time(:millisecond) >= deadline do
          {:timeout, attempts + 1}
        else
          Process.sleep(5)
          poll_wm_item(id, deadline, attempts + 1)
        end

      item ->
        {:ok, item}
    end
  end
end
