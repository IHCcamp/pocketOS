defmodule Input do
  def start do
    {:ok, pid} = :gen_server.start(__MODULE__, [], [])
    display_module = :erlang.whereis(:display)
    :gen_server.call(display_module, {:listen, pid}, 60000)
    {:ok, pid}
  end

  def init(_) do
    {:ok, [parser_module: nil, parser_state: nil]}
  end

  def set_parser_module(:keyboard_parser) do
    :gen_server.call(:input, {:set_parser_module, KeyboardParser})
  end

  def handle_call({:set_parser_module, module}, _from, state) do
    new_state =
      state
      |> Keyword.put(:parser_module, module)
      |> Keyword.put(:parser_state, :erlang.apply(module, :init_state, []))

    {:reply, :ok, new_state}
  end

  def handle_info({:keyboard_event, _key_code, _key_down, _event_ts} = event, state) do
    parser_module = Keyword.fetch!(state, :parser_module)
    parser_state = Keyword.fetch!(state, :parser_state)

    new_parser_state = :erlang.apply(parser_module, :process_event, [parser_state, event])
    new_state = Keyword.put(state, :parser_state, new_parser_state)

    {:noreply, new_state}
  end

  def terminate(_reason, _state), do: :ok
end
