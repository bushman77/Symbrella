defmodule Llm.Colony.BootstrapTest do
  use ExUnit.Case, async: false
  import Bitwise

  alias Llm.Colony.Bootstrap

  setup do
    old = Application.get_env(:llm, :colony)
    Application.put_env(:llm, :colony, enabled?: true, mode: :observe)

    path =
      Path.join(
        Path.expand("../../../priv", __DIR__),
        ".colony_bootstrap_test_#{System.unique_integer([:positive, :monotonic])}"
      )

    on_exit(fn ->
      File.rm(path)
      if is_nil(old),
        do: Application.delete_env(:llm, :colony),
        else: Application.put_env(:llm, :colony, old)
    end)

    {:ok, path: path}
  end

  test "missing credential does not silently create a replacement identity", %{path: path} do
    assert {:error, :registration_not_configured} =
             Bootstrap.ensure_key(key_file: path, api_key: nil, username: nil)

    refute File.exists?(path)
  end

  test "existing environment credential is saved without contacting Colony", %{path: path} do
    key = "col_test_existing_key_123456"

    assert {:ok, :environment_key_saved} =
             Bootstrap.ensure_key(
               key_file: path,
               api_key: key,
               username: nil,
               request: fn _, _ -> flunk("must not register") end
             )

    assert String.trim(File.read!(path)) == key
    assert {:ok, %File.Stat{mode: mode}} = File.stat(path)
    assert (mode &&& 0o077) == 0
  end

  test "registration saves key before confirming, without exposing it in result", %{path: path} do
    key = "col_test_generated_key_ABCDEF"
    parent = self()

    request = fn
      "/auth/register/begin", %{
        username: "symbrella-test",
        display_name: "Symbrella",
        bio: _
      } ->
        {:ok, %{"api_key" => key, "claim_token" => "claim-secret"}}

      "/auth/register/confirm", %{
        claim_token: "claim-secret",
        key_fingerprint: "ABCDEF"
      } ->
        send(parent, {:readback_at_confirmation, String.trim(File.read!(path))})
        {:ok, %{"status" => "active"}}
    end

    assert {:ok, :registered} =
             Bootstrap.ensure_key(
               key_file: path,
               api_key: nil,
               username: "symbrella-test",
               display_name: "Symbrella",
               request: request
             )

    assert_receive {:readback_at_confirmation, ^key}
    assert {:ok, %File.Stat{mode: mode}} = File.stat(path)
    assert (mode &&& 0o077) == 0
  end

  test "an existing account key is never overwritten", %{path: path} do
    old_key = "col_old_existing_key_123456"
    File.write!(path, old_key <> "\n")
    File.chmod!(path, 0o600)

    assert {:error, :credential_sources_conflict} =
             Bootstrap.ensure_key(
               key_file: path,
               api_key: "col_different_key_123456",
               username: "new-agent",
               request: fn _, _ -> flunk("must not register") end
             )

    assert String.trim(File.read!(path)) == old_key
  end

  test "insecure file permissions stop provisioning rather than replacing the key", %{path: path} do
    File.write!(path, "col_old_existing_key_123456\n")
    File.chmod!(path, 0o644)

    assert {:error, :unsafe_key_file} =
             Bootstrap.ensure_key(
               key_file: path,
               api_key: nil,
               username: "new-agent",
               request: fn _, _ -> flunk("must not register") end
             )
  end

  test "a username conflict leaves no key file and creates no fallback agent", %{path: path} do
    assert {:error, :username_unavailable} =
             Bootstrap.ensure_key(
               key_file: path,
               api_key: nil,
               username: "symbrella",
               request: fn "/auth/register/begin", _ -> {:error, :username_unavailable} end
             )

    refute File.exists?(path)
  end

  test "failed confirmation retains issued key for recovery instead of replacing identity", %{path: path} do
    key = "col_test_generated_key_XYZ123"

    request = fn
      "/auth/register/begin", _ ->
        {:ok, %{"api_key" => key, "claim_token" => "claim"}}

      "/auth/register/confirm", _ ->
        {:error, :registration_transport_error}
    end

    assert {:error, :registration_transport_error} =
             Bootstrap.ensure_key(
               key_file: path,
               api_key: nil,
               username: "symbrella-test",
               request: request
             )

    assert String.trim(File.read!(path)) == key
  end
end
