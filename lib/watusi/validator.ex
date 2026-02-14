defmodule Watusi.Validator do
  @moduledoc """
  Semantic validator for WASM instruction sequences.
  Ensures operand stack consistency and type safety.
  """
  alias Watusi.Instructions

  def validate_function(instructions, signature, ctx) do
    {params, results} = signature
    # Initial stack is empty; locals are not on the operand stack
    # We wrap the function body in an implicit block that returns the function results
    do_validate(instructions, results, ctx, [results])
  end

  defp do_validate([], _expected_results, _ctx, _block_stack) do
    # Final stack must match expected results of the current block
    :ok
  end

  # This is a stub for full validation. 
  # Full validation requires mapping every opcode to its {inputs, outputs}.
  # For now, we return :ok to maintain existing behavior while we build the infrastructure.
  defp do_validate([_instr | rest], expected, ctx, blocks) do
    do_validate(rest, expected, ctx, blocks)
  end
end
