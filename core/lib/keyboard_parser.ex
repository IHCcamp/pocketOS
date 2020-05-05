defmodule KeyboardParser do
  @moduledoc """
  Documentation for Input.
  """

  @key_timeout_ms 800

  @key_1 ?1
  @key_2 ?2
  @key_3 ?3
  @key_4 ?4
  @key_5 ?5
  @key_6 ?6
  @key_7 ?7
  @key_8 ?8
  @key_9 ?9
  @key_0 ?0

  @key_1_choices ~c(.,:!?";) |> List.to_tuple()
  @key_2_choices ~c(abc) |> List.to_tuple()
  @key_3_choices ~c(def) |> List.to_tuple()
  @key_4_choices ~c(ghi) |> List.to_tuple()
  @key_5_choices ~c(jkl) |> List.to_tuple()
  @key_6_choices ~c(mno) |> List.to_tuple()
  @key_7_choices ~c(pqrs) |> List.to_tuple()
  @key_8_choices ~c(tuv) |> List.to_tuple()
  @key_9_choices ~c(xywz) |> List.to_tuple()
  @key_0_choices ~c( ) |> List.to_tuple()

  def start do
    init_state()
  end

  def init_state do
    [
      buffer: '',
      last_timestamp: 0,
      upper_case: false,
      last_key: '',
      char_index: 0,
      key_down: false
    ]
  end

  def process_event(state, {:keyboard_event, key_code, true, event_timestamp} = event) do
    buffer = Keyword.get(state, :buffer)

    if is_new_character?(state, event) do
      state
      |> Keyword.put(:char_index, 0)
      |> Keyword.put(:last_timestamp, event_timestamp)
      |> Keyword.put(:last_key, key_code)
      |> Keyword.put(:key_down, true)
      |> Keyword.put(:buffer, buffer ++ [char_from_key(key_code, 0)])
    else
      key_count = increase_key_count(state)

      chr = char_from_key(key_code, key_count)
      new_buffer = replace_last(buffer, chr)

      state
      |> Keyword.put(:char_index, key_count)
      |> Keyword.put(:last_timestamp, event_timestamp)
      |> Keyword.put(:last_key, key_code)
      |> Keyword.put(:key_down, true)
      |> Keyword.put(:buffer, new_buffer)
    end
  end

  def process_event(state, {:keyboard_event, key_code, false, event_timestamp}) do
    Keyword.put(state, :key_down, false)
  end

  def process_event(state, {:keyboard_event, _key_code, _key_down, _event_timestamp}) do
    state
  end

  defp replace_last([last], replace_with) do
    [replace_with]
  end

  defp replace_last([h | t], replace_with) do
    [h | replace_last(t, replace_with)]
  end

  defp time_ms_diff(prev_t, curr_t) do
    curr_t - prev_t
  end

  defp timeout?(delta_t), do: delta_t > @key_timeout_ms

  defp same_key_code?(prev_key_code, curr_key_code), do: prev_key_code == curr_key_code

  defp is_new_character?(state, {:keyboard_event, key_code, true, timestamp}) do
    last_timestamp = Keyword.fetch!(state, :last_timestamp)
    last_key = Keyword.fetch!(state, :last_key)

    timed_out =
      time_ms_diff(last_timestamp, timestamp)
      |> timeout?()

    same_key = same_key_code?(last_key, key_code)

    case {timed_out, same_key} do
      {false, true} ->
        false

      _ ->
        true
    end
  end

  defp is_new_character?(_state, {:keyboard_event, _key_code, false, _timestamp}) do
    false
  end

  defp increase_key_count(state) do
    state[:char_index] + 1
  end

  defp char_from_key(@key_1, index) do
    size = tuple_size(@key_1_choices)
    @key_1_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_2, index) do
    size = tuple_size(@key_2_choices)
    @key_2_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_3, index) do
    size = tuple_size(@key_3_choices)
    @key_3_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_4, index) do
    size = tuple_size(@key_4_choices)
    @key_4_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_5, index) do
    size = tuple_size(@key_5_choices)
    @key_5_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_6, index) do
    size = tuple_size(@key_6_choices)
    @key_6_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_7, index) do
    size = tuple_size(@key_7_choices)
    @key_7_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_8, index) do
    size = tuple_size(@key_8_choices)
    @key_8_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_9, index) do
    size = tuple_size(@key_9_choices)
    @key_9_choices |> elem(rem(index, size))
  end

  defp char_from_key(@key_0, index) do
    size = tuple_size(@key_0_choices)
    @key_0_choices |> elem(rem(index, size))
  end

  defp char_from_key(_, _) do
    :erlang.display("Maybe not exactly what you want")
    ''
  end
end
