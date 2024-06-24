defmodule ErlmudClient do
  @moduledoc """
  Documentation for `ErlmudClient`.
  """

  @doc """
  Hello world.

  ## Examples

   iex> ErlmudClient.hello()
   :world

  """
  def hello do
    :world
  end

  def input_loop(server_pid) do
    input = IO.gets("> ")
    # IO.puts :gen_server.call(server_pid, {:cmd, input})
    IO.puts GenServer.call(server_pid, {:cmd, input})
    input_loop(server_pid)
  end

  def event_output_loop do
    receive do
      {:output, message} ->
        IO.puts(message)
    end
    event_output_loop()
  end

  def start_client(server_pid) do
    Node.set_cookie(:erlMUD)
    spawn_link(__MODULE__, :input_loop, [server_pid])
    event_pid = spawn_link(__MODULE__, :event_output_loop, [])
    GenServer.cast(server_pid, {:set_event_pid, event_pid})
  end

end
