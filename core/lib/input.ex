defmodule Input do
  def start do
    {:ok, pid} = :gen_server.start(__MODULE__, [], [])
    display_module = :erlang.whereis(:display)
    :erlang.display(display_module)
    :gen_server.call(display_module, {:listen, pid}, 60000)
    {:ok, pid}
  end

  def init(_) do
    {:ok, [parser_module: nil, parser_state: nil]}
  end

  def set_parser_module(:keyboard_parser) do
    :erlang.display("setting parser module...")
    :gen_server.call(:input, {:set_parser_module, KeyboardParser})
  end

  def handle_call({:set_parser_module, module}, _from, state) do
    :erlang.display("handling the call")

    new_state =
      state
      |> Keyword.put(:parser_module, module)
      |> Keyword.put(:parser_state, :erlang.apply(module, :init_state, []))

    :erlang.display(new_state)
    {:reply, :ok, new_state}
  end

  def handle_info({:keyboard_event, key_code, key_down, event_ts} = event, state) do
    :erlang.display(key_code)
    {:noreply, state}
  end

  def terminate(_reason, _state), do: :ok
end
