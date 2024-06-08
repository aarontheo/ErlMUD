defmodule ErlmudClientTest do
  use ExUnit.Case
  doctest ErlmudClient

  test "greets the world" do
    assert ErlmudClient.hello() == :world
  end
end
