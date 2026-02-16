defmodule Watusi.Validator do
  @moduledoc false
  alias Watusi.Instructions

  def validate_function(instructions, signature, ctx) do
    # results is a list of expected return types
    {_params, results} = signature

    # Control stack frame: {expected_results, initial_stack_size}
    # For the function body, we expect 'results' and start with empty stack.
    do_validate(instructions, [], ctx, [{results, 0}])
  end

  # End of instruction sequence
  defp do_validate([], stack, _ctx, [{expected, start_size} | _]) do
    # Current stack (minus what was there before the block) must match expected results
    actual = Enum.take(stack, length(stack) - start_size)

    if types_match?(actual, expected) do
      :ok
    else
      raise "Type mismatch at end of block: expected #{inspect(expected)}, got #{inspect(actual)}"
    end
  end

  # Standard instruction
  defp do_validate([{:instr, name, args, _labels} | rest], stack, ctx, blocks) do
    case name do
      "block" ->
        # Push new control frame
        {_, expected} = decode_blocktype(args, ctx)
        do_validate(rest, stack, ctx, [{expected, length(stack)} | blocks])

      "end" ->
        # Pop control frame and verify its result
        [{expected, start_size} | outer_blocks] = blocks
        actual = Enum.take(stack, length(stack) - start_size)

        if types_match?(actual, expected) do
          # The outer block continues with the results of this block on its stack
          new_stack = Enum.drop(stack, length(actual)) ++ expected
          do_validate(rest, new_stack, ctx, outer_blocks)
        else
          raise "Type mismatch at 'end': expected #{inspect(expected)}, got #{inspect(actual)}"
        end

      _ ->
        {inputs, outputs} = Instructions.meta(name)
        # 1. Pop and verify inputs
        new_stack = pop_and_verify(stack, Enum.reverse(inputs))
        # 2. Push outputs (resolving polymorphic types if necessary)
        final_stack = new_stack ++ resolve_outputs(outputs, args, ctx)
        do_validate(rest, final_stack, ctx, blocks)
    end
  end

  defp pop_and_verify(stack, []), do: stack

  defp pop_and_verify([actual | rest], [expected | expected_rest]) do
    if type_matches?(actual, expected) do
      pop_and_verify(rest, expected_rest)
    else
      raise "Type mismatch: expected #{expected}, got #{actual}"
    end
  end

  defp pop_and_verify([], [expected | _]), do: raise("Stack underflow: expected #{expected}")

  defp type_matches?(_actual, :any), do: true
  defp type_matches?(t, t), do: true
  defp type_matches?(_, _), do: false

  defp types_match?(actual, expected) do
    # In WASM, the stack grows to the right, so [i32, f64] means i32 is bottom.
    # Comparison should be exact.
    actual == expected
  end

  defp resolve_outputs(outputs, _args, _ctx) do
    # In a full implementation, this would handle local.get types etc.
    # For now, we convert atom types to our internal representation.
    outputs
  end

  defp decode_blocktype(_args, _ctx) do
    # Stub: returns {params, results}
    {[], []}
  end
end
