defmodule Watusi.Encoder.Instructions do
  @moduledoc false
  alias Watusi.Encoder.Common
  alias Watusi.Encoder.Sections
  alias Watusi.Instructions
  alias Watusi.LEB128
  import Bitwise

  @control_flow_ops ["block", "loop", "if", "try", "try_table"]
  @branch_ops ["br", "br_if", "br_table"]
  @call_ops ["call", "call_indirect", "return_call", "return_call_indirect"]
  @global_ops ["global.get", "global.set"]
  @tag_ops ["throw", "rethrow", "catch", "catch_ref"]
  @f32_overflow_midpoint (1 <<< 128) - (1 <<< 103)
  @simd_shapes ["i8x16", "i16x8", "i32x4", "i64x2", "f32x4", "f64x2"]

  @bulk_mem_ops [
    "memory.init",
    "data.drop",
    "memory.copy",
    "memory.fill",
    "table.init",
    "elem.drop",
    "table.copy"
  ]

  @memory_ops [
    "i32.load",
    "i64.load",
    "f32.load",
    "f64.load",
    "i32.store",
    "i64.store",
    "f32.store",
    "f64.store",
    "i32.load8_s",
    "i32.load8_u",
    "i32.load16_s",
    "i32.load16_u",
    "i64.load8_s",
    "i64.load8_u",
    "i64.load16_s",
    "i64.load16_u",
    "i64.load32_s",
    "i64.load32_u",
    "i32.store8",
    "i32.store16",
    "i64.store8",
    "i64.store16",
    "i64.store32",
    "v128.load",
    "v128.load8x8_s",
    "v128.load8x8_u",
    "v128.load16x4_s",
    "v128.load16x4_u",
    "v128.load32x2_s",
    "v128.load32x2_u",
    "v128.load8_splat",
    "v128.load16_splat",
    "v128.load32_splat",
    "v128.load64_splat",
    "v128.store",
    "v128.load8_lane",
    "v128.load16_lane",
    "v128.load32_lane",
    "v128.load64_lane",
    "v128.store8_lane",
    "v128.store16_lane",
    "v128.store32_lane",
    "v128.store64_lane",
    "v128.load32_zero",
    "v128.load64_zero",
    "i32.atomic.load",
    "i64.atomic.load",
    "i32.atomic.load8_u",
    "i32.atomic.load16_u",
    "i64.atomic.load8_u",
    "i64.atomic.load16_u",
    "i64.atomic.load32_u",
    "i32.atomic.store",
    "i64.atomic.store",
    "i32.atomic.store8",
    "i32.atomic.store16",
    "i64.atomic.store8",
    "i64.atomic.store16",
    "i64.atomic.store32",
    "i32.atomic.rmw.add",
    "i64.atomic.rmw.add",
    "i32.atomic.rmw8.add_u",
    "i32.atomic.rmw16.add_u",
    "i64.atomic.rmw8.add_u",
    "i64.atomic.rmw16.add_u",
    "i64.atomic.rmw32.add_u",
    "i32.atomic.rmw.sub",
    "i64.atomic.rmw.sub",
    "i32.atomic.rmw8.sub_u",
    "i32.atomic.rmw16.sub_u",
    "i64.atomic.rmw8.sub_u",
    "i64.atomic.rmw16.sub_u",
    "i64.atomic.rmw32.sub_u",
    "i32.atomic.rmw.and",
    "i64.atomic.rmw.and",
    "i32.atomic.rmw8.and_u",
    "i32.atomic.rmw16.and_u",
    "i64.atomic.rmw8.and_u",
    "i64.atomic.rmw16.and_u",
    "i64.atomic.rmw32.and_u",
    "i32.atomic.rmw.or",
    "i64.atomic.rmw.or",
    "i32.atomic.rmw8.or_u",
    "i32.atomic.rmw16.or_u",
    "i64.atomic.rmw8.or_u",
    "i64.atomic.rmw16.or_u",
    "i64.atomic.rmw32.or_u",
    "i32.atomic.rmw.xor",
    "i64.atomic.rmw.xor",
    "i32.atomic.rmw8.xor_u",
    "i32.atomic.rmw16.xor_u",
    "i64.atomic.rmw8.xor_u",
    "i64.atomic.rmw16.xor_u",
    "i64.atomic.rmw32.xor_u",
    "i32.atomic.rmw.xchg",
    "i64.atomic.rmw.xchg",
    "i32.atomic.rmw8.xchg_u",
    "i32.atomic.rmw16.xchg_u",
    "i64.atomic.rmw8.xchg_u",
    "i64.atomic.rmw16.xchg_u",
    "i64.atomic.rmw32.xchg_u",
    "i32.atomic.rmw.cmpxchg",
    "i64.atomic.rmw.cmpxchg",
    "i32.atomic.rmw8.cmpxchg_u",
    "i32.atomic.rmw16.cmpxchg_u",
    "i64.atomic.rmw8.cmpxchg_u",
    "i64.atomic.rmw16.cmpxchg_u",
    "i64.atomic.rmw32.cmpxchg_u",
    "memory.atomic.notify",
    "memory.atomic.wait32",
    "memory.atomic.wait64"
  ]

  @simd_ops [
    "v128.load",
    "v128.load8x8_s",
    "v128.load8x8_u",
    "v128.load16x4_s",
    "v128.load16x4_u",
    "v128.load32x2_s",
    "v128.load32x2_u",
    "v128.load8_splat",
    "v128.load16_splat",
    "v128.load32_splat",
    "v128.load64_splat",
    "v128.store",
    "v128.const",
    "i8x16.shuffle",
    "i8x16.swizzle",
    "i8x16.splat",
    "i16x8.splat",
    "i32x4.splat",
    "i64x2.splat",
    "f32x4.splat",
    "f64x2.splat",
    "i8x16.extract_lane_s",
    "i8x16.extract_lane_u",
    "i8x16.replace_lane",
    "i16x8.extract_lane_s",
    "i16x8.extract_lane_u",
    "i16x8.replace_lane",
    "i32x4.extract_lane",
    "i32x4.replace_lane",
    "i64x2.extract_lane",
    "i64x2.replace_lane",
    "f32x4.extract_lane",
    "f32x4.replace_lane",
    "f64x2.extract_lane",
    "f64x2.replace_lane",
    "i8x16.eq",
    "i8x16.ne",
    "i8x16.lt_s",
    "i8x16.lt_u",
    "i8x16.gt_s",
    "i8x16.gt_u",
    "i8x16.le_s",
    "i8x16.le_u",
    "i8x16.ge_s",
    "i8x16.ge_u",
    "i16x8.eq",
    "i16x8.ne",
    "i16x8.lt_s",
    "i16x8.lt_u",
    "i16x8.gt_s",
    "i16x8.gt_u",
    "i16x8.le_s",
    "i16x8.le_u",
    "i16x8.ge_s",
    "i16x8.ge_u",
    "i32x4.eq",
    "i32x4.ne",
    "i32x4.lt_s",
    "i32x4.lt_u",
    "i32x4.gt_s",
    "i32x4.gt_u",
    "i32x4.le_s",
    "i32x4.le_u",
    "i32x4.ge_s",
    "i32x4.ge_u",
    "f32x4.eq",
    "f32x4.ne",
    "f32x4.lt",
    "f32x4.gt",
    "f32x4.le",
    "f32x4.ge",
    "f64x2.eq",
    "f64x2.ne",
    "f64x2.lt",
    "f64x2.gt",
    "f64x2.le",
    "f64x2.ge",
    "v128.not",
    "v128.and",
    "v128.andnot",
    "v128.or",
    "v128.xor",
    "v128.bitselect",
    "v128.any_true",
    "v128.load8_lane",
    "v128.load16_lane",
    "v128.load32_lane",
    "v128.load64_lane",
    "v128.store8_lane",
    "v128.store16_lane",
    "v128.store32_lane",
    "v128.store64_lane",
    "v128.load32_zero",
    "v128.load64_zero",
    "f32x4.demote_f64x2_zero",
    "f64x2.promote_low_f32x4",
    "i8x16.abs",
    "i8x16.neg",
    "i8x16.popcnt",
    "i8x16.all_true",
    "i8x16.bitmask",
    "i8x16.narrow_i16x8_s",
    "i8x16.narrow_i16x8_u",
    "f32x4.ceil",
    "f32x4.floor",
    "f32x4.trunc",
    "f32x4.nearest",
    "i8x16.shl",
    "i8x16.shr_s",
    "i8x16.shr_u",
    "i8x16.add",
    "i8x16.add_sat_s",
    "i8x16.add_sat_u",
    "i8x16.sub",
    "i8x16.sub_sat_s",
    "i8x16.sub_sat_u",
    "f64x2.ceil",
    "f64x2.floor",
    "i8x16.min_s",
    "i8x16.min_u",
    "i8x16.max_s",
    "i8x16.max_u",
    "f64x2.trunc",
    "i8x16.avgr_u",
    "i16x8.extadd_pairwise_i8x16_s",
    "i16x8.extadd_pairwise_i8x16_u",
    "i32x4.extadd_pairwise_i16x8_s",
    "i32x4.extadd_pairwise_i16x8_u",
    "i16x8.abs",
    "i16x8.neg",
    "i16x8.q15mulr_sat_s",
    "i16x8.all_true",
    "i16x8.bitmask",
    "i16x8.narrow_i32x4_s",
    "i16x8.narrow_i32x4_u",
    "i16x8.extend_low_i8x16_s",
    "i16x8.extend_high_i8x16_s",
    "i16x8.extend_low_i8x16_u",
    "i16x8.extend_high_i8x16_u",
    "i16x8.shl",
    "i16x8.shr_s",
    "i16x8.shr_u",
    "i16x8.add",
    "i16x8.add_sat_s",
    "i16x8.add_sat_u",
    "i16x8.sub",
    "i16x8.sub_sat_s",
    "i16x8.sub_sat_u",
    "f64x2.nearest",
    "i16x8.mul",
    "i16x8.min_s",
    "i16x8.min_u",
    "i16x8.max_s",
    "i16x8.max_u",
    "f64x2.sqrt",
    "i16x8.avgr_u",
    "i16x8.extmul_low_i8x16_s",
    "i16x8.extmul_high_i8x16_s",
    "i16x8.extmul_low_i8x16_u",
    "i16x8.extmul_high_i8x16_u",
    "i32x4.abs",
    "i32x4.neg",
    "i32x4.all_true",
    "i32x4.bitmask",
    "i32x4.extend_low_i16x8_s",
    "i32x4.extend_high_i16x8_s",
    "i32x4.extend_low_i16x8_u",
    "i32x4.extend_high_i16x8_u",
    "i32x4.shl",
    "i32x4.shr_s",
    "i32x4.shr_u",
    "i32x4.add",
    "i32x4.sub",
    "i32x4.mul",
    "i32x4.min_s",
    "i32x4.min_u",
    "i32x4.max_s",
    "i32x4.max_u",
    "i32x4.dot_i16x8_s",
    "i32x4.extmul_low_i16x8_s",
    "i32x4.extmul_high_i16x8_s",
    "i32x4.extmul_low_i16x8_u",
    "i32x4.extmul_high_i16x8_u",
    "i64x2.abs",
    "i64x2.neg",
    "i64x2.all_true",
    "i64x2.bitmask",
    "i64x2.extend_low_i32x4_s",
    "i64x2.extend_high_i32x4_s",
    "i64x2.extend_low_i32x4_u",
    "i64x2.extend_high_i32x4_u",
    "i64x2.shl",
    "i64x2.shr_s",
    "i64x2.shr_u",
    "i64x2.add",
    "i64x2.sub",
    "i64x2.mul",
    "i64x2.eq",
    "i64x2.ne",
    "i64x2.lt_s",
    "i64x2.gt_s",
    "i64x2.le_s",
    "i64x2.ge_s",
    "i64x2.extmul_low_i32x4_s",
    "i64x2.extmul_high_i32x4_s",
    "i64x2.extmul_low_i32x4_u",
    "i64x2.extmul_high_i32x4_u",
    "f32x4.abs",
    "f32x4.neg",
    "f32x4.sqrt",
    "f32x4.add",
    "f32x4.sub",
    "f32x4.mul",
    "f32x4.div",
    "f32x4.min",
    "f32x4.max",
    "f32x4.pmin",
    "f32x4.pmax",
    "f64x2.abs",
    "f64x2.neg",
    "f64x2.sqrt",
    "f64x2.add",
    "f64x2.sub",
    "f64x2.mul",
    "f64x2.div",
    "f64x2.min",
    "f64x2.max",
    "f64x2.pmin",
    "f64x2.pmax",
    "i32x4.trunc_sat_f32x4_s",
    "i32x4.trunc_sat_f32x4_u",
    "f32x4.convert_i32x4_s",
    "f32x4.convert_i32x4_u",
    "i32x4.trunc_sat_f64x2_s_zero",
    "i32x4.trunc_sat_f64x2_u_zero",
    "f64x2.convert_low_i32x4_s",
    "f64x2.convert_low_i32x4_u",
    "i8x16.relaxed_swizzle",
    "i16x8.relaxed_q15mulr_s",
    "i16x8.dot_i8x16_i7x16_s",
    "i32x4.relaxed_trunc_f32x4_s",
    "i32x4.relaxed_trunc_f32x4_u",
    "i32x4.relaxed_trunc_f64x2_s_zero",
    "i32x4.relaxed_trunc_f64x2_u_zero",
    "f32x4.relaxed_madd",
    "f32x4.relaxed_nmadd",
    "f64x2.relaxed_madd",
    "f64x2.relaxed_nmadd",
    "i8x16.relaxed_laneselect",
    "i16x8.relaxed_laneselect",
    "i32x4.relaxed_laneselect",
    "i64x2.relaxed_laneselect",
    "f32x4.relaxed_min",
    "f32x4.relaxed_max",
    "f64x2.relaxed_min",
    "f64x2.relaxed_max",
    "i16x8.relaxed_dot_i8x16_i7x16_s",
    "i16x8.relaxed_dot_i8x16_i7x16_add_s",
    "i32x4.relaxed_dot_i8x16_i7x16_add_s",
    "i32x4.relaxed_dot_i8x16_i7x16_s"
  ]

  @atomic_ops [
    "memory.atomic.notify",
    "memory.atomic.wait32",
    "memory.atomic.wait64",
    "i32.atomic.load",
    "i64.atomic.load",
    "i32.atomic.load8_u",
    "i32.atomic.load16_u",
    "i64.atomic.load8_u",
    "i64.atomic.load16_u",
    "i64.atomic.load32_u",
    "i32.atomic.store",
    "i64.atomic.store",
    "i32.atomic.store8",
    "i32.atomic.store16",
    "i64.atomic.store8",
    "i64.atomic.store16",
    "i64.atomic.store32",
    "i32.atomic.rmw.add",
    "i64.atomic.rmw.add",
    "i32.atomic.rmw8.add_u",
    "i32.atomic.rmw16.add_u",
    "i64.atomic.rmw8.add_u",
    "i64.atomic.rmw16.add_u",
    "i64.atomic.rmw32.add_u",
    "i32.atomic.rmw.sub",
    "i64.atomic.rmw.sub",
    "i32.atomic.rmw8.sub_u",
    "i32.atomic.rmw16.sub_u",
    "i64.atomic.rmw8.sub_u",
    "i64.atomic.rmw16.sub_u",
    "i64.atomic.rmw32.sub_u",
    "i32.atomic.rmw.and",
    "i64.atomic.rmw.and",
    "i32.atomic.rmw8.and_u",
    "i32.atomic.rmw16.and_u",
    "i64.atomic.rmw8.and_u",
    "i64.atomic.rmw16.and_u",
    "i64.atomic.rmw32.and_u",
    "i32.atomic.rmw.or",
    "i64.atomic.rmw.or",
    "i32.atomic.rmw8.or_u",
    "i32.atomic.rmw16.or_u",
    "i64.atomic.rmw8.or_u",
    "i64.atomic.rmw16.or_u",
    "i64.atomic.rmw32.or_u",
    "i32.atomic.rmw.xor",
    "i64.atomic.rmw.xor",
    "i32.atomic.rmw8.xor_u",
    "i32.atomic.rmw16.xor_u",
    "i64.atomic.rmw8.xor_u",
    "i64.atomic.rmw16.xor_u",
    "i64.atomic.rmw32.xor_u",
    "i32.atomic.rmw.xchg",
    "i64.atomic.rmw.xchg",
    "i32.atomic.rmw8.xchg_u",
    "i32.atomic.rmw16.xchg_u",
    "i64.atomic.rmw8.xchg_u",
    "i64.atomic.rmw16.xchg_u",
    "i64.atomic.rmw32.xchg_u",
    "i32.atomic.rmw.cmpxchg",
    "i64.atomic.rmw.cmpxchg",
    "i32.atomic.rmw8.cmpxchg_u",
    "i32.atomic.rmw16.cmpxchg_u",
    "i64.atomic.rmw8.cmpxchg_u",
    "i64.atomic.rmw16.cmpxchg_u",
    "i64.atomic.rmw32.cmpxchg_u"
  ]

  @table_ops ["table.get", "table.set", "table.grow", "table.size", "table.fill"]

  @gc_ops [
    "struct.new",
    "struct.new_default",
    "struct.get",
    "struct.get_s",
    "struct.get_u",
    "struct.set",
    "array.new",
    "array.new_default",
    "array.new_fixed",
    "array.get",
    "array.set",
    "array.len",
    "ref.test",
    "ref.cast",
    "br_on_cast",
    "any.convert_extern",
    "extern.convert_any"
  ]

  @immediate_types [:int, :float, :id, :offset, :align, :binary]

  # Guards for instruction categories
  defguard is_control_flow(name) when name in @control_flow_ops
  defguard is_branch(name) when name in @branch_ops
  defguard is_call(name) when name in @call_ops
  defguard is_global_op(name) when name in @global_ops
  defguard is_bulk_mem(name) when name in @bulk_mem_ops
  defguard is_memory_op(name) when name in @memory_ops
  defguard is_simd(name) when name in @simd_ops
  defguard is_atomic(name) when name in @atomic_ops
  defguard is_table_op(name) when name in @table_ops
  defguard is_tag_op(name) when name in @tag_ops
  defguard is_gc_op(name) when name in @gc_ops

  @labeled_terminators ["else", "end"]
  defguardp is_labeled_terminator(name) when name in @labeled_terminators

  @memory_mgmt_ops ["memory.grow", "memory.size"]
  defguardp is_memory_mgmt_op(name) when name in @memory_mgmt_ops

  @dynamic_target_ops ["br", "br_if", "call", "return_call", "catch", "rethrow"]
  defguardp is_dynamic_target_op(name) when name in @dynamic_target_ops

  @signature_definition_kinds ["param", "result", "type"]
  defguardp is_signature_definition(name) when name in @signature_definition_kinds

  @struct_mgmt_ops ["struct.get", "struct.get_s", "struct.get_u", "struct.set"]
  defguardp is_struct_mgmt_op(name) when name in @struct_mgmt_ops

  @if_branch_metadata ["then", "else"]
  defguardp is_if_branch_metadata(name) when name in @if_branch_metadata

  @catch_handler_keywords ["catch", "catch_all", "catch_ref", "catch_all_ref"]
  defguardp is_catch_handler_keyword(name) when name in @catch_handler_keywords

  @try_catch_keywords ["catch", "catch_all"]

  @collect_arg_keywords ["memory", "type", "table"] ++ @catch_handler_keywords
  defguardp is_collect_arg_keyword(name) when name in @collect_arg_keywords

  @ignored_instruction_nodes @signature_definition_kinds ++
                               @if_branch_metadata ++
                               @try_catch_keywords ++
                               ["local", "export", "delegate", "do", "memory", "try"]
  defguardp is_ignored_instruction_node(kind) when kind in @ignored_instruction_nodes

  def encode_instruction({:instr, name, args, labels}, ctx) do
    case name do
      "select" -> encode_select(args)
      _ -> encode_standard_instruction(name, args, ctx, labels)
    end
  end

  defp encode_select([]), do: [0x1B]

  defp encode_select(args) do
    # Extract result types for typed select
    types =
      Enum.flat_map(args, fn
        [{:keyword, "result"} | ts] -> ts
        _ -> []
      end)
      |> Enum.map(fn {:keyword, t} -> Instructions.valtype(t) end)

    [0x1C, Common.encode_vector(types, & &1)]
  end

  defp encode_standard_instruction(name, args, ctx, labels) do
    immediates = encode_immediates(name, args, ctx, labels)

    case Instructions.opcode(name) do
      {:fc, op} -> [<<0xFC>>, Common.encode_u32(op) | immediates]
      {:fd, op} -> [<<0xFD>>, Common.encode_u32(op) | immediates]
      {:fe, op} -> [<<0xFE>>, Common.encode_u32(op) | immediates]
      {:fb, op} -> [<<0xFB>>, Common.encode_u32(op) | immediates]
      opcode -> [opcode | immediates]
    end
  end

  defp encode_immediates(name, args, ctx, _labels) when is_atomic(name) do
    # Atomic operations are specialized memory operations that sometimes
    # require a 'dummy' memory index (currently always 0 in WASM Core 1.0)
    encode_atomic_immediates(name, args, ctx)
  end

  defp encode_immediates(name, _args, _ctx, _labels) when is_labeled_terminator(name) do
    []
  end

  defp encode_immediates("try_table", args, ctx, labels) do
    # try_table has a blocktype AND a catch vector
    {bt_args, catch_args} = Enum.split_with(args, &(!match?({:binary, _}, &1)))

    [
      encode_control_flow_immediates(bt_args, ctx),
      Enum.map(catch_args, &encode_arg("try_table", &1, ctx, labels))
    ]
  end

  defp encode_immediates(name, args, ctx, _labels) when is_control_flow(name) do
    # block/loop/if/try require a blocktype immediate
    encode_control_flow_immediates(args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_simd(name) do
    # SIMD instructions have specialized immediates (lanes, constants)
    encode_simd_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_memory_op(name) do
    # Standard memory operations (load/store) have alignment and offset immediates
    encode_mem_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_bulk_mem(name) do
    # Bulk memory instructions (copy/fill/init) have various index immediates
    encode_bulk_mem_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_table_op(name) do
    # table.get/set require a table index
    encode_table_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_tag_op(name) do
    # throw/rethrow require a tag index
    encode_tag_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_gc_op(name) do
    # GC instructions like struct.new/get require type and field indices
    encode_gc_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels)
       when is_memory_mgmt_op(name) do
    memidx =
      case args do
        [[{:keyword, "memory"}, {:id, id}] | _] ->
          resolve_index(id, ctx.memories, ctx.imports, "memory")

        [[{:keyword, "memory"}, {:int, i}] | _] ->
          i

        [{:id, id} | _] ->
          resolve_index(id, ctx.memories, ctx.imports, "memory")

        [{:int, i} | _] ->
          i

        _ ->
          0
      end

    [Common.encode_u32(memidx)]
  end

  defp encode_immediates(name, args, ctx, labels)
       when is_branch(name) or is_call(name) or name == "br_table" or name == "catch" or
              name == "rethrow" do
    # These target labels, functions, or tags
    # Special handling for indirect calls and br_table which need custom encoding
    case name in ["call_indirect", "return_call_indirect", "br_table"] do
      true -> encode_dynamic_immediates(name, args, ctx, labels)
      false -> Enum.map(args, &encode_arg(name, &1, ctx, labels))
    end
  end

  defp encode_immediates(name, args, ctx, labels) do
    # Catch-all for instructions with simple immediates
    Enum.map(args, &encode_arg(name, &1, ctx, labels))
  end

  defp encode_dynamic_immediates("br_table", args, _ctx, labels) do
    # br_table expects a vector of targets and a default target
    indices =
      Enum.map(args, fn
        {:id, id} -> resolve_label(id, labels)
        {:int, i} -> i
      end)

    case indices do
      [] ->
        raise "br_table needs target"

      list ->
        {ts, [d]} = Enum.split(list, -1)
        [Common.encode_vector(ts, &Common.encode_u32/1), Common.encode_u32(d)]
    end
  end

  defp encode_dynamic_immediates("call_indirect", args, ctx, _labels) do
    # call_indirect needs the type index and the table index
    # (call_indirect (type $t) (table $tab) ...) or (call_indirect (type $t) ...)
    type_idx = resolve_indirect_type_idx(args, ctx)
    table_idx = resolve_indirect_table_idx(args, ctx)

    [Common.encode_u32(type_idx), Common.encode_u32(table_idx)]
  end

  defp encode_dynamic_immediates("return_call_indirect", args, ctx, _labels) do
    # tail-call variant of call_indirect
    type_idx = resolve_indirect_type_idx(args, ctx)
    table_idx = resolve_indirect_table_idx(args, ctx)

    [Common.encode_u32(type_idx), Common.encode_u32(table_idx)]
  end

  defp encode_dynamic_immediates(name, args, ctx, labels)
       when name in ["br", "br_if", "call", "return_call", "catch", "rethrow"] do
    Enum.map(args, &encode_arg(name, &1, ctx, labels))
  end

  defp resolve_indirect_type_idx(args, ctx) do
    case Enum.find(args, &match?([{:keyword, "type"}, _], &1)) do
      [{:keyword, "type"}, {:id, id}] ->
        resolve_type_id(id, ctx)

      [{:keyword, "type"}, {:int, i}] ->
        i

      _ ->
        {params, results} = extract_param_and_result_types(args)

        sig = {params, results}

        case Enum.find_index(ctx.sigs, &(&1 == sig)) do
          nil -> raise("Type not found for call_indirect signature: #{inspect(sig)}")
          idx -> idx
        end
    end
  end

  defp resolve_indirect_table_idx(args, ctx) do
    case Enum.find(args, &match?([{:keyword, "table"}, _], &1)) do
      [{:keyword, "table"}, {:id, id}] ->
        resolve_index(id, ctx.tables, ctx.imports, "table")

      [{:keyword, "table"}, {:int, i}] ->
        i

      _ ->
        args
        |> Enum.reject(&match?([{:keyword, k} | _] when is_signature_definition(k), &1))
        |> case do
          [{:id, id} | _] -> resolve_index(id, ctx.tables, ctx.imports, "table")
          [{:int, i} | _] -> i
          _ -> 0
        end
    end
  end

  defp encode_control_flow_immediates(args, ctx) do
    # Block types can be void (0x40), a single valtype, or a type index (signed LEB)
    case args do
      [{:int, bt}] ->
        [bt]

      _ ->
        {params, results} = extract_block_signature(args)

        has_explicit_params = params != []

        case Enum.find(args, &match?([{:keyword, "type"}, _], &1)) do
          [{:keyword, "type"}, {:id, id}] ->
            {type_params, type_results} =
              Sections.extract_raw_signature(Enum.at(ctx.types, resolve_type_pos(id, ctx)))

            effective_params = resolve_block_params(params, type_params)
            effective_results = resolve_block_results(results, type_results)

            encode_blocktype(
              effective_params,
              effective_results,
              ctx,
              effective_params != []
            )

          [{:keyword, "type"}, {:int, i}] ->
            {type_params, type_results} = Sections.extract_raw_signature(Enum.at(ctx.types, i))
            effective_params = resolve_block_params(params, type_params)
            effective_results = resolve_block_results(results, type_results)

            encode_blocktype(
              effective_params,
              effective_results,
              ctx,
              effective_params != []
            )

          _ ->
            encode_blocktype(params, results, ctx, has_explicit_params)
        end
    end
  end

  defp resolve_type_pos(id, ctx) do
    Enum.find_index(ctx.types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
      raise("Type not found: $#{id}")
  end

  defp resolve_block_params([], fallback), do: fallback
  defp resolve_block_params(params, _fallback), do: params

  defp resolve_block_results([], fallback), do: fallback
  defp resolve_block_results(results, _fallback), do: results

  defp encode_blocktype(params, results, ctx, force_param_sig?) do
    case {params, results} do
      {[], []} -> [0x40]
      {[], [type]} -> [Instructions.valtype(type)]
      {[_ | _], _} -> handle_param_blocktype(params, results, ctx, force_param_sig?)
      sig -> encode_blocktype_sig(sig, ctx)
    end
  end

  defp handle_param_blocktype(params, results, ctx, force_param_sig?) do
    case {results, force_param_sig?} do
      {[], true} -> encode_blocktype_sig({params, []}, ctx)
      {[], false} -> [0x40]
      {[type], true} -> encode_blocktype_sig({params, [type]}, ctx)
      {[type], false} -> [Instructions.valtype(type)]
      {_, _} -> encode_blocktype_sig({params, results}, ctx)
    end
  end

  defp encode_blocktype_sig(sig, ctx) do
    idx =
      Enum.find_index(ctx.sigs, &(&1 == sig)) ||
        raise("Type not found for block: #{inspect(sig)}")

    [LEB128.encode_signed(idx)]
  end

  defp extract_block_signature(args) do
    args
    |> Enum.take_while(fn
      {:id, _} -> true
      [{:keyword, k} | _] when is_signature_definition(k) -> true
      _ -> false
    end)
    |> extract_param_and_result_types()
  end

  defp extract_param_and_result_types(args) do
    {params, results} =
      Enum.reduce(args, {[], []}, fn
        [{:keyword, "param"} | ts], {params_acc, results_acc} ->
          param_types =
            Enum.reduce(ts, [], fn
              {:id, _}, acc -> acc
              {:keyword, t}, acc -> [t | acc]
              other, acc -> [other | acc]
            end)

          {param_types ++ params_acc, results_acc}

        [{:keyword, "result"} | ts], {params_acc, results_acc} ->
          result_types =
            Enum.reduce(ts, [], fn
              {:keyword, t}, acc -> [t | acc]
              other, acc -> [other | acc]
            end)

          {params_acc, result_types ++ results_acc}

        _, acc ->
          acc
      end)

    {Enum.reverse(params), Enum.reverse(results)}
  end

  defp encode_tag_immediates(_name, args, ctx) do
    case args do
      [{:id, id} | _] ->
        [Common.encode_u32(resolve_index(id, ctx.tags, ctx.imports, "tag"))]

      [{:int, i} | _] ->
        [Common.encode_u32(i)]

      _ ->
        [Common.encode_u32(0)]
    end
  end

  defp encode_gc_immediates(name, args, ctx) do
    # 1. Resolve type index (first immediate for most GC instructions)
    type_idx =
      case Enum.find(args, &match?({:id, _}, &1)) do
        {:id, id} ->
          resolve_type_id(id, ctx)

        _ ->
          case Enum.find(args, &match?({:int, _}, &1)) do
            {:int, i} -> i
            _ -> 0
          end
      end

    # 2. Resolve field index (second immediate for struct.get/set)
    case name do
      n when is_struct_mgmt_op(n) ->
        field_idx = resolve_field_index(type_idx, args, ctx)
        [Common.encode_u32(type_idx), Common.encode_u32(field_idx)]

      _ ->
        [Common.encode_u32(type_idx)]
    end
  end

  defp resolve_field_index(type_idx, args, ctx) do
    # Fields can be named or indexed.
    # We find the struct type definition to resolve symbolic field IDs.
    type_item = Enum.at(ctx.types, type_idx)

    case Enum.find(args, fn
           {:id, id} -> !String.starts_with?(id, "$") or not type_is_id?(id, ctx)
           _ -> false
         end) do
      {:id, id} ->
        # Find the field index in the struct definition
        [[{:keyword, "struct"} | fields] | _] =
          case type_item do
            [{:keyword, "type"}, {:id, _} | rest] -> rest
            [{:keyword, "type"} | rest] -> rest
          end

        Enum.find_index(fields, fn
          [{:keyword, "field"}, {:id, ^id} | _] -> true
          _ -> false
        end) || raise("Field not found: $#{id} in struct type #{type_idx}")

      _ ->
        # Fallback to integer index
        case Enum.filter(args, &match?({:int, _}, &1)) do
          [_, {:int, i} | _] -> i
          _ -> 0
        end
    end
  end

  defp type_is_id?(id, ctx) do
    Enum.any?(ctx.types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1))
  end

  defp encode_table_immediates(_name, args, ctx) do
    case args do
      [[{:keyword, "table"}, {:id, id}] | _] ->
        [Common.encode_u32(resolve_index(id, ctx.tables, ctx.imports, "table"))]

      [[{:keyword, "table"}, {:int, i}] | _] ->
        [Common.encode_u32(i)]

      [{:id, id} | _] ->
        [Common.encode_u32(resolve_index(id, ctx.tables, ctx.imports, "table"))]

      [{:int, i} | _] ->
        [Common.encode_u32(i)]

      _ ->
        [Common.encode_u32(0)]
    end
  end

  defp encode_atomic_immediates(name, args, ctx) do
    # Atomic instructions (load, store, RMW, notify, wait) all use standard
    # memory immediates (alignment and offset). They do NOT encode a memory
    # index in the current WASM specification.
    encode_mem_immediates(name, args, ctx)
  end

  defp encode_simd_immediates(name, args, ctx) when is_memory_op(name) do
    # SIMD memory instructions include load/store, load splat, load zero,
    # and lane-specific loads/stores.
    is_lane_op = String.contains?(name, "_lane")

    immediates = encode_mem_immediates(name, args, ctx)

    case is_lane_op do
      true ->
        lane =
          Enum.reduce(args, 0, fn
            {:int, i}, _acc -> i
            _, acc -> acc
          end)

        immediates ++ [lane]

      false ->
        immediates
    end
  end

  defp encode_simd_immediates(name, args, _ctx)
       when name in [
              "i8x16.extract_lane_s",
              "i8x16.extract_lane_u",
              "i8x16.replace_lane",
              "i16x8.extract_lane_s",
              "i16x8.extract_lane_u",
              "i16x8.replace_lane",
              "i32x4.extract_lane",
              "i32x4.replace_lane",
              "i64x2.extract_lane",
              "i64x2.replace_lane",
              "f32x4.extract_lane",
              "f32x4.replace_lane",
              "f64x2.extract_lane",
              "f64x2.replace_lane"
            ] do
    case Enum.find(args, &match?({:int, _}, &1)) do
      {:int, lane} -> [lane]
      _ -> [0]
    end
  end

  defp encode_simd_immediates("i8x16.shuffle", args, _ctx) do
    # 16 integer immediates
    Enum.reduce(args, [], fn
      {:int, i}, acc -> [i | acc]
      _, acc -> acc
    end)
    |> Enum.reverse()
  end

  defp encode_simd_immediates("v128.const", args, _ctx) do
    # v128.const can be initialized from various shapes (i8x16, i32x4, etc.)
    encode_v128_const(args)
  end

  defp encode_simd_immediates(_name, _args, _ctx), do: []

  defp encode_v128_const(args) do
    shape_type =
      case Enum.find(args, fn
             {:keyword, k} -> k in @simd_shapes
             _ -> false
           end) do
        {:keyword, k} -> k
        _ -> nil
      end

    values =
      Enum.filter(args, fn
        {:int, _} -> true
        {:float, _} -> true
        {:float, _, _} -> true
        _ -> false
      end)

    case {shape_type, length(values)} do
      {shape, len} when len in [2, 4, 8, 16] ->
        encode_v128_values(shape, len, values)

      other ->
        raise "Invalid v128.const arguments: #{inspect(other)}"
    end
    |> IO.iodata_to_binary()
    |> List.wrap()
  end

  defp encode_v128_values(_, 16, values), do: Enum.map(values, fn {:int, v} -> <<v::8>> end)

  defp encode_v128_values(_, 8, values),
    do: Enum.map(values, fn {:int, v} -> <<v::little-16>> end)

  defp encode_v128_values("f32x4", 4, values) do
    Enum.map(values, fn
      {:int, v} -> encode_f32(v * 1.0)
      {:float, v} -> encode_f32(v)
      {:float, v, literal} -> encode_f32_decimal(v, literal)
    end)
  end

  defp encode_v128_values(_, 4, values) do
    Enum.map(values, fn
      {:int, v} -> <<v::little-32>>
      {:float, v} -> encode_f32(v)
      {:float, v, literal} -> encode_f32_decimal(v, literal)
    end)
  end

  defp encode_v128_values("f64x2", 2, values) do
    Enum.map(values, fn
      {:int, v} -> encode_f64(v * 1.0)
      {:float, v} -> encode_f64(v)
      {:float, v, _} -> encode_f64(v)
    end)
  end

  defp encode_v128_values(_, 2, values) do
    Enum.map(values, fn
      {:int, v} -> <<v::little-64>>
      {:float, v} -> encode_f64(v)
      {:float, v, _} -> encode_f64(v)
    end)
  end

  defp encode_bulk_mem_immediates("memory.init", args, ctx) do
    # memory.init $data or memory.init $memory $data
    # Binary: dataidx memoryidx
    {memidx, dataidx} =
      case args do
        [a, b | _] ->
          {resolve_mem_idx(a, ctx), resolve_data_index(b, ctx.data)}

        [a] ->
          {0, resolve_data_index(a, ctx.data)}

        _ ->
          {0, 0}
      end

    [Common.encode_u32(dataidx), Common.encode_u32(memidx)]
  end

  defp encode_bulk_mem_immediates("data.drop", args, ctx) do
    dataidx =
      case args do
        [a | _] -> resolve_data_index(a, ctx.data)
        _ -> 0
      end

    [Common.encode_u32(dataidx)]
  end

  defp encode_bulk_mem_immediates("memory.fill", args, ctx) do
    memidx =
      case args do
        [[{:keyword, "memory"}, m_arg] | _] -> resolve_mem_idx(m_arg, ctx)
        [a | _] -> resolve_mem_idx(a, ctx)
        _ -> 0
      end

    [Common.encode_u32(memidx)]
  end

  defp encode_bulk_mem_immediates("memory.copy", args, ctx) do
    # memory.copy $dst $src or memory.copy (memory $dst) (memory $src)
    # Binary: dstidx srcidx
    {dst, src} =
      case args do
        [[{:keyword, "memory"}, d_arg], [{:keyword, "memory"}, s_arg] | _] ->
          {resolve_mem_idx(d_arg, ctx), resolve_mem_idx(s_arg, ctx)}

        [a, b | _] ->
          {resolve_mem_idx(a, ctx), resolve_mem_idx(b, ctx)}

        _ ->
          {0, 0}
      end

    [Common.encode_u32(dst), Common.encode_u32(src)]
  end

  defp encode_bulk_mem_immediates("table.init", args, ctx) do
    # table.init $table $elem or table.init $elem
    # Binary: elemidx tableidx
    {tableidx, elemidx} =
      case args do
        [a, b | _] ->
          {resolve_table_idx(a, ctx), resolve_elem_index(b, ctx.elems)}

        [a] ->
          {0, resolve_elem_index(a, ctx.elems)}

        _ ->
          {0, 0}
      end

    [Common.encode_u32(elemidx), Common.encode_u32(tableidx)]
  end

  defp encode_bulk_mem_immediates("elem.drop", args, ctx) do
    elemidx =
      case args do
        [a | _] -> resolve_elem_index(a, ctx.elems)
        _ -> 0
      end

    [Common.encode_u32(elemidx)]
  end

  defp encode_bulk_mem_immediates("table.copy", args, ctx) do
    # table.copy $dst $src
    # Binary: dstidx srcidx
    {dst, src} =
      case args do
        [a, b | _] ->
          {resolve_table_idx(a, ctx), resolve_table_idx(b, ctx)}

        _ ->
          {0, 0}
      end

    [Common.encode_u32(dst), Common.encode_u32(src)]
  end

  defp resolve_table_idx([{:keyword, "table"}, arg], ctx), do: resolve_table_idx(arg, ctx)
  defp resolve_table_idx({:id, id}, ctx), do: resolve_index(id, ctx.tables, ctx.imports, "table")
  defp resolve_table_idx({:int, i}, _), do: i
  defp resolve_table_idx(_, _), do: 0

  defp resolve_mem_idx([{:keyword, "memory"}, arg], ctx), do: resolve_mem_idx(arg, ctx)
  defp resolve_mem_idx({:id, id}, ctx), do: resolve_index(id, ctx.memories, ctx.imports, "memory")
  defp resolve_mem_idx({:int, i}, _), do: i
  defp resolve_mem_idx(_, _), do: 0

  defp encode_mem_immediates(name, args, ctx) do
    # 1. Resolve memory index. Defaults to 0 (the primary memory).
    # In Multi-Memory, an explicit (memory $idx) can be provided.
    mem_idx = resolve_memory_index(name, args, ctx)

    # 2. Determine if the targeted memory is 64-bit.
    # Memory64 uses 64-bit offsets in the binary encoding.
    is_64? = memory_is_64?(mem_idx, ctx)

    offset = resolve_offset(args)
    align = resolve_align(name, args)

    # 3. Encode flags (alignment + extension bits).
    # Bit 6 (0x40) indicates that a memory index follows.
    flags =
      case mem_idx > 0 do
        true -> align ||| 0x40
        false -> align
      end

    encoded_offset =
      case is_64? do
        true -> LEB128.encode_unsigned(offset)
        false -> Common.encode_u32(offset)
      end

    case mem_idx > 0 do
      true ->
        [Common.encode_u32(flags), Common.encode_u32(mem_idx), encoded_offset]

      false ->
        [Common.encode_u32(flags), encoded_offset]
    end
  end

  defp resolve_offset(args) do
    case Enum.find(args, &match?({:offset, _}, &1)) do
      {:offset, v} -> v
      _ -> 0
    end
  end

  defp resolve_align(name, args) do
    case Enum.find(args, &match?({:align, _}, &1)) do
      {:align, v} -> to_log2(v)
      _ -> natural_align(name)
    end
  end

  defp resolve_memory_index(name, args, ctx) do
    mem_node = Enum.find(args, &match?([{:keyword, "memory"} | _], &1))
    positional_mem = resolve_positional_mem(name, args)

    case {mem_node, positional_mem} do
      {[{:keyword, "memory"}, {:id, id}], _} ->
        resolve_index(id, ctx.memories, ctx.imports, "memory")

      {[{:keyword, "memory"}, {:int, i}], _} ->
        i

      {nil, {:id, id}} ->
        resolve_index(id, ctx.memories, ctx.imports, "memory")

      {nil, {:int, i}} ->
        i

      _ ->
        0
    end
  end

  defp resolve_positional_mem(name, args) do
    case String.contains?(name, "_lane") do
      true -> resolve_lane_positional_mem(args)
      false -> resolve_standard_positional_mem(name, args)
    end
  end

  defp resolve_lane_positional_mem(args) do
    case args do
      [{:id, id}, {:int, _} | _] -> {:id, id}
      [{:int, i}, {:int, _} | _] -> {:int, i}
      [{:id, id} | rest] -> maybe_resolve_lane({:id, id}, rest)
      [{:int, i} | rest] -> maybe_resolve_lane({:int, i}, rest)
      _ -> nil
    end
  end

  defp maybe_resolve_lane(val, rest) do
    case Enum.any?(rest, &match?({:int, _}, &1)) do
      true -> val
      false -> nil
    end
  end

  defp resolve_standard_positional_mem(name, args) do
    case args do
      [{:id, id} | _] ->
        {:id, id}

      [{:int, i} | _] ->
        # Avoid mistaking SIMD lane immediates for memory indices.
        case String.contains?(name, ".load") or String.contains?(name, ".store") or
               String.contains?(name, "atomic") do
          true -> {:int, i}
          false -> nil
        end

      _ ->
        nil
    end
  end

  defp memory_is_64?(idx, ctx) do
    # We find the memory definition at the given index (accounting for imports).
    memory_imports =
      Enum.filter(ctx.imports, fn item ->
        case Sections.normalize_import(item) do
          {_, _, "memory", _} -> true
          _ -> false
        end
      end)

    import_count = length(memory_imports)

    case idx < import_count do
      true ->
        import_item = Enum.at(memory_imports, idx)
        {_, _, _, rest} = Sections.normalize_import(import_item)
        Enum.any?(rest, &match?({:keyword, "i64"}, &1))

      false ->
        case Enum.at(ctx.memories, idx - import_count) do
          [{:keyword, "memory"} | rest] -> Enum.any?(rest, &match?({:keyword, "i64"}, &1))
          _ -> false
        end
    end
  end

  defp natural_align(name) do
    # Default alignment is determined by the size of the load/store operation.
    # We check smaller/narrower widths first to correctly handle instructions
    # like i64.load32_u where the operation size (32) is smaller than the type (64).
    case String.contains?(name, "lane") do
      true -> natural_align_lane(name)
      false -> natural_align_standard(name)
    end
  end

  defp natural_align_lane(name) do
    cond do
      String.contains?(name, "8_lane") -> 0
      String.contains?(name, "16_lane") -> 1
      String.contains?(name, "32_lane") -> 2
      String.contains?(name, "64_lane") -> 3
      true -> 0
    end
  end

  defp natural_align_standard(name) do
    cond do
      String.contains?(name, "v128") -> 4
      String.contains?(name, "8") -> 0
      String.contains?(name, "16") -> 1
      String.contains?(name, "32") -> 2
      String.contains?(name, "64") -> 3
      String.contains?(name, "notify") -> 2
      true -> 0
    end
  end

  defp to_log2(1), do: 0
  defp to_log2(2), do: 1
  defp to_log2(4), do: 2
  defp to_log2(8), do: 3
  defp to_log2(16), do: 4
  defp to_log2(v), do: raise("Invalid align: #{v}")

  defp encode_arg("ref.null", {:keyword, "extern"}, _, _), do: Instructions.valtype("externref")
  defp encode_arg("ref.null", {:keyword, "func"}, _, _), do: Instructions.valtype("funcref")
  defp encode_arg("ref.null", {:keyword, type}, _, _), do: Instructions.valtype(type)

  defp encode_arg(name, {:id, id}, _, labels) when is_branch(name),
    do: resolve_label(id, labels) |> Common.encode_u32()

  defp encode_arg("i32.const", {:int, val}, _, _), do: val |> wrap_i32() |> LEB128.encode_signed()

  defp encode_arg("i32.const", {:float, val}, _, _),
    do: trunc(val) |> wrap_i32() |> LEB128.encode_signed()

  defp encode_arg("i64.const", {:int, val}, _, _), do: val |> wrap_i64() |> LEB128.encode_signed()

  defp encode_arg("i64.const", {:float, val}, _, _),
    do: trunc(val) |> wrap_i64() |> LEB128.encode_signed()

  defp encode_arg("f32.const", {:float, :neg_nan}, _, _), do: <<0, 0, 192, 255>>

  defp encode_arg("f32.const", {:float, {:nan, sign, payload}}, _, _) do
    bits = encode_sign(sign) <<< 31 ||| 0xFF <<< 23 ||| (payload &&& 0x7FFFFF)
    <<bits::little-32>>
  end

  defp encode_arg("f32.const", {:float, val, literal}, _, _), do: encode_f32_decimal(val, literal)

  defp encode_arg("f32.const", {:float, val}, _, _), do: encode_f32(val)
  defp encode_arg("f32.const", {:int, val}, _, _), do: encode_f32(val * 1.0)

  defp encode_arg("f64.const", {:float, :neg_nan}, _, _), do: <<0, 0, 0, 0, 0, 0, 248, 255>>

  defp encode_arg("f64.const", {:float, {:nan, sign, payload}}, _, _) do
    bits = encode_sign(sign) <<< 63 ||| 0x7FF <<< 52 ||| (payload &&& 0xFFFFFFFFFFFFF)
    <<bits::little-64>>
  end

  defp encode_arg("f64.const", {:float, val, _literal}, _, _), do: encode_f64(val)
  defp encode_arg("f64.const", {:float, val}, _, _), do: encode_f64(val)
  defp encode_arg("f64.const", {:int, val}, _, _), do: encode_f64(val * 1.0)

  defp encode_arg("ref.func", {:id, id}, ctx, _),
    do: Common.encode_u32(resolve_index(id, ctx.funcs, ctx.imports, "func"))

  defp encode_arg(name, [{:keyword, k} | _], _ctx, _labels)
       when name in ["call_indirect", "return_call_indirect"] and k in ["type", "param", "result"],
       do: []

  defp encode_arg(name, {:id, id}, ctx, _labels)
       when is_dynamic_target_op(name),
       do:
         Common.encode_u32(
           resolve_index(id, get_list_for_kind(name, ctx), ctx.imports, get_kind_for_op(name))
         )

  defp encode_arg(name, {:id, id}, ctx, _) when is_global_op(name),
    do: Common.encode_u32(resolve_index(id, ctx.globals, ctx.imports, "global"))

  defp encode_arg(_name, {:binary, bin}, _, _), do: bin
  defp encode_arg(_name, {:int, val}, _, _), do: Common.encode_u32(val)
  defp encode_arg(_name, {:id, id}, ctx, _), do: Common.encode_u32(Map.fetch!(ctx.local_map, id))

  defp get_list_for_kind(name, ctx) when name in ["call", "return_call"], do: ctx.funcs
  defp get_list_for_kind(name, ctx) when name in ["throw", "catch", "rethrow"], do: ctx.tags

  defp get_kind_for_op(name) when name in ["call", "return_call"], do: "func"
  defp get_kind_for_op(name) when name in ["throw", "catch", "rethrow"], do: "tag"

  defp wrap_i32(val) do
    <<signed::signed-32>> = <<val::unsigned-32>>
    signed
  end

  defp wrap_i64(val) do
    <<signed::signed-64>> = <<val::unsigned-64>>
    signed
  end

  defp encode_sign(-1), do: 1
  defp encode_sign(_), do: 0

  defp resolve_label(id, _labels) when is_integer(id), do: id

  defp resolve_label(id, labels) do
    # Labels are resolved to their relative depth in the label stack
    Enum.find_index(labels, &(&1 == id)) || raise("Label not found: $#{id}")
  end

  defp resolve_type_id(id, ctx) do
    Enum.find_index(ctx.types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
      raise("Type not found: $#{id}")
  end

  defp encode_f32(:infinity), do: <<0, 0, 128, 127>>
  defp encode_f32(:neg_infinity), do: <<0, 0, 128, 255>>
  defp encode_f32(:nan), do: <<0, 0, 192, 127>>
  defp encode_f32(:neg_nan), do: <<0, 0, 192, 255>>
  defp encode_f32(f), do: <<f::float-little-size(32)>>

  defp encode_f64(:infinity), do: <<0, 0, 0, 0, 0, 0, 240, 127>>
  defp encode_f64(:neg_infinity), do: <<0, 0, 0, 0, 0, 0, 240, 255>>
  defp encode_f64(:nan), do: <<0, 0, 0, 0, 0, 0, 248, 127>>
  defp encode_f64(:neg_nan), do: <<0, 0, 0, 0, 0, 0, 248, 255>>
  defp encode_f64(f), do: <<f::float-little-size(64)>>

  defp encode_f32_decimal(value, literal) do
    case parse_decimal_rational(literal) do
      {:ok, {num, den}} ->
        value
        |> nearest_f32_bits(num, den)
        |> encode_f32_bits()

      :error ->
        encode_f32(value)
    end
  end

  defp nearest_f32_bits(value, num, den) do
    bits = float_to_f32_bits(value)

    case bits do
      0x7F80_0000 -> handle_f32_overflow(bits, num, den)
      0xFF80_0000 -> handle_f32_overflow(bits, -num, den)
      _ -> find_best_f32_candidate(bits, num, den)
    end
  end

  defp handle_f32_overflow(bits, num, den) do
    case compare_to_midpoint(num, den) do
      :lt -> if bits == 0x7F80_0000, do: 0x7F7F_FFFF, else: 0xFF7F_FFFF
      _ -> bits
    end
  end

  defp find_best_f32_candidate(bits, num, den) do
    [bits - 1, bits, bits + 1]
    |> Enum.filter(&finite_f32_bits?/1)
    |> Enum.uniq()
    |> Enum.reduce(bits, fn cand, best ->
      case compare_candidate(cand, best, num, den) do
        :lt -> cand
        :eq -> pick_even_tie(cand, best)
        :gt -> best
      end
    end)
  end

  defp compare_to_midpoint(num, den) do
    lhs = abs(num)
    rhs = @f32_overflow_midpoint * den

    case lhs < rhs do
      true ->
        :lt

      false ->
        case lhs > rhs do
          true -> :gt
          false -> :eq
        end
    end
  end

  defp compare_candidate(cand_bits, best_bits, num, den) do
    {cand_num, cand_den} = f32_bits_to_rational(cand_bits)
    {best_num, best_den} = f32_bits_to_rational(best_bits)

    cand_dist = abs(num * cand_den - cand_num * den)
    best_dist = abs(num * best_den - best_num * den)

    lhs = cand_dist * best_den
    rhs = best_dist * cand_den

    case lhs < rhs do
      true ->
        :lt

      false ->
        case lhs > rhs do
          true -> :gt
          false -> :eq
        end
    end
  end

  defp pick_even_tie(cand, best) do
    case rem(cand, 2) do
      0 -> cand
      _ -> best
    end
  end

  defp finite_f32_bits?(bits),
    do: bits >= 0 and bits <= 0xFFFF_FFFF and (bits &&& 0x7F80_0000) != 0x7F80_0000

  defp float_to_f32_bits(value) do
    <<bits::little-32>> = <<value::float-little-size(32)>>
    bits
  end

  defp encode_f32_bits(bits), do: <<bits::little-32>>

  defp f32_bits_to_rational(bits) do
    sign =
      case bits >>> 31 do
        0 -> 1
        _ -> -1
      end

    exp = bits >>> 23 &&& 0xFF
    frac = bits &&& 0x7F_FFFF

    case exp do
      0 ->
        case frac do
          0 -> {0, 1}
          _ -> make_pow2_rational(sign * frac, -149)
        end

      _ ->
        make_pow2_rational(sign * ((1 <<< 23) + frac), exp - 150)
    end
  end

  defp make_pow2_rational(num, exp2) when exp2 >= 0, do: {num <<< exp2, 1}
  defp make_pow2_rational(num, exp2), do: {num, 1 <<< -exp2}

  defp parse_decimal_rational(literal) when is_binary(literal) do
    cleaned = String.replace(literal, "_", "")

    {sign, rest} =
      case cleaned do
        <<"-", tail::binary>> -> {-1, tail}
        <<"+", tail::binary>> -> {1, tail}
        _ -> {1, cleaned}
      end

    case String.split(rest, ~r/[eE]/, parts: 2) do
      [mantissa, exponent_str] ->
        case Integer.parse(exponent_str) do
          {exponent, ""} -> decimal_mantissa_to_rational(sign, mantissa, exponent)
          _ -> :error
        end

      [mantissa] ->
        decimal_mantissa_to_rational(sign, mantissa, 0)
    end
  rescue
    _ -> :error
  end

  defp parse_decimal_rational(_), do: :error

  defp decimal_mantissa_to_rational(sign, mantissa, exponent) do
    case String.split(mantissa, ".", parts: 2) do
      [whole, frac] ->
        digits = normalize_decimal_digits(whole <> frac)
        decimal_digits_to_rational(sign, digits, exponent - byte_size(frac))

      [whole] ->
        digits = normalize_decimal_digits(whole)
        decimal_digits_to_rational(sign, digits, exponent)
    end
  end

  defp decimal_digits_to_rational(sign, digits, exp10) do
    int = sign * String.to_integer(digits)

    case exp10 >= 0 do
      true -> {:ok, {int * pow10(exp10), 1}}
      false -> {:ok, {int, pow10(-exp10)}}
    end
  end

  defp normalize_decimal_digits(""), do: "0"

  defp normalize_decimal_digits(digits) do
    case String.trim_leading(digits, "0") do
      "" -> "0"
      trimmed -> trimmed
    end
  end

  defp pow10(0), do: 1
  defp pow10(n) when n > 0, do: 10 * pow10(n - 1)

  def resolve_index({:id, id}, list, imports, kind), do: resolve_index(id, list, imports, kind)

  def resolve_index(id, list, imports, kind) do
    # Identifiers in WASM are resolved in an index space that is shared
    # between imports and local definitions. Imports always come first.
    kind_imports =
      Enum.filter(imports, fn
        [{:keyword, "import"}, _, _, [{:keyword, ^kind} | _]] ->
          true

        [{:keyword, ^kind} | rest] ->
          Enum.any?(rest, fn
            [{:keyword, "import"} | _] -> true
            _ -> false
          end)

        _ ->
          false
      end)

    case find_id_in_imports(kind_imports, kind, id) do
      nil ->
        # If not found in imports, check the local definitions
        length(kind_imports) +
          (find_local_index(list, kind, id) || raise("ID not found: #{kind} $#{id}"))

      idx ->
        idx
    end
  end

  defp find_id_in_imports(imports, kind, id) do
    Enum.find_index(imports, fn item ->
      case item do
        [{:keyword, "import"}, _, _, [{:keyword, ^kind}, {:id, ^id} | _]] ->
          true

        [{:keyword, ^kind}, {:id, ^id} | rest] ->
          Enum.any?(rest, &match?([{:keyword, "import"} | _], &1))

        _ ->
          false
      end
    end)
  end

  defp find_local_index(ls, k, id),
    do: Enum.find_index(ls, &match?([{:keyword, ^k}, {:id, ^id} | _], &1))

  defp resolve_data_index([{:keyword, "data"}, arg], data), do: resolve_data_index(arg, data)

  defp resolve_data_index({:id, id}, data),
    do:
      Enum.find_index(data, &match?([{:keyword, "data"}, {:id, ^id} | _], &1)) ||
        raise("Data ID not found: $#{id}")

  defp resolve_data_index({:int, i}, _), do: i
  defp resolve_data_index(_, _), do: 0

  defp resolve_elem_index([{:keyword, "elem"}, arg], elems), do: resolve_elem_index(arg, elems)

  defp resolve_elem_index({:id, id}, elems),
    do:
      Enum.find_index(elems, &match?([{:keyword, "elem"}, {:id, ^id} | _], &1)) ||
        raise("Elem ID not found: $#{id}")

  defp resolve_elem_index({:int, i}, _), do: i
  defp resolve_elem_index(_, _), do: 0

  @simd_shapes ["i8x16", "i16x8", "i32x4", "i64x2", "f32x4", "f64x2"]

  @stop_keywords ["end", "else", "catch", "catch_all", "delegate"]
  defguardp is_stop_keyword(name) when name in @stop_keywords

  def collect_args([{:keyword, name} | _] = rest, acc) when is_stop_keyword(name),
    do: {Enum.reverse(acc), rest}

  def collect_args([{type, _, _} = arg | rest], acc) when type in @immediate_types,
    do: collect_args(rest, [arg | acc])

  def collect_args([{type, _} = arg | rest], acc) when type in @immediate_types,
    do: collect_args(rest, [arg | acc])

  def collect_args([{:keyword, shape} = arg | rest], acc) when shape in @simd_shapes,
    do: collect_args(rest, [arg | acc])

  def collect_args([[{:keyword, "result"} | _] = arg | rest], acc),
    do: collect_args(rest, [arg | acc])

  def collect_args([[{:keyword, "param"} | _] = arg | rest], acc),
    do: collect_args(rest, [arg | acc])

  def collect_args([[{:keyword, name} | _] = arg | rest], acc) when is_collect_arg_keyword(name),
    do: collect_args(rest, [arg | acc])

  def collect_args(rest, acc), do: {Enum.reverse(acc), rest}

  def build_local_map([{:keyword, "func"} | rest], types \\ [], signatures \\ []) do
    # Locals (params and local declarations) are indexed consecutively
    body =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    # Check if function uses a type reference to determine starting index
    starting_idx = resolve_starting_local_idx(body, types, signatures)

    {_, map} =
      Enum.reduce(body, {starting_idx, %{}}, fn
        [{:keyword, kind} | ts], {idx, acc} when kind in ["param", "local"] ->
          # Each param/local declaration can have multiple items: IDs and types
          # We need to count how many actual params/locals are declared
          # For example: (param $x i32) declares 1 param, (param i32 i32) declares 2
          {final_idx, final_acc} =
            Enum.reduce(ts, {idx, acc}, fn
              {:id, id}, {i, a} ->
                # Assign current index to this ID, then increment
                {i, Map.put(a, id, i)}

              {:keyword, _}, {i, a} ->
                # Type keyword - increment index
                {i + 1, a}

              _, state ->
                state
            end)

          {final_idx, final_acc}

        _, state ->
          state
      end)

    map
  end

  defp resolve_starting_local_idx(body, types, signatures) do
    case Enum.find(body, &match?([{:keyword, "type"}, _], &1)) do
      [{:keyword, "type"}, {:id, id}] ->
        type_item = Enum.find(types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1))

        case type_item do
          nil ->
            0

          _ ->
            {params, _} = Sections.extract_raw_signature(type_item)
            length(params)
        end

      [{:keyword, "type"}, {:int, i}] ->
        case Enum.at(signatures, i) do
          {params, _} -> length(params)
          _ -> 0
        end

      _ ->
        0
    end
  end

  def collect_instructions(rest, ctx, label_stack \\ []) do
    # Instructions are collected into a flat list, resolving identifiers
    # and nesting into a linear sequence of WASM opcodes.
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    do_collect_instructions(rest, ctx, [], label_stack)
  end

  defp do_collect_instructions([], _ctx, acc, _labels), do: Enum.reverse(acc)

  # Skip export declarations
  defp do_collect_instructions([[{:keyword, "export"}, _] | rest], ctx, acc, labels) do
    do_collect_instructions(rest, ctx, acc, labels)
  end

  defp do_collect_instructions([[{:keyword, "try_table"} | args] | rest], ctx, acc, labels) do
    # try_table $id (result ...) (catch ...) ... end
    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        other -> {nil, other}
      end

    new_labels = [label | labels]

    blocktype_list = encode_control_flow_immediates(args, ctx)

    blocktype =
      case blocktype_list do
        [bt] -> bt
        list -> hd(list)
      end

    # Separate catch handlers from body instructions
    {catches, body_and_other} =
      Enum.split_with(args, fn
        [{:keyword, k} | _] when is_catch_handler_keyword(k) -> true
        _ -> false
      end)

    inner_rest =
      Enum.drop_while(body_and_other, fn
        [{:keyword, k} | _] when is_signature_definition(k) or is_catch_handler_keyword(k) -> true
        _ -> false
      end)

    inner_instrs = collect_instructions(inner_rest, ctx, new_labels)
    catch_encoded = Common.encode_vector(catches, &encode_catch(&1, ctx, labels))

    acc = [{:instr, "try_table", [{:int, blocktype}, {:binary, catch_encoded}], labels} | acc]
    acc = Enum.reduce(inner_instrs, acc, fn instr, a -> [instr | a] end)
    new_acc = [{:instr, "end", [], new_labels} | acc]

    do_collect_instructions(rest, ctx, new_acc, labels)
  end

  defp do_collect_instructions([{:keyword, "try_table"} | rest], ctx, acc, labels) do
    # Flat-style try_table: try_table $id (result ...) (catch ...) ... end
    {args, remaining} = collect_args(rest, [])

    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        other -> {nil, other}
      end

    new_labels = [label | labels]

    blocktype_list = encode_control_flow_immediates(args, ctx)

    blocktype =
      case blocktype_list do
        [bt] -> bt
        list -> hd(list)
      end

    {catches, _} =
      Enum.split_with(args, fn
        [{:keyword, k} | _] when is_catch_handler_keyword(k) -> true
        _ -> false
      end)

    catch_encoded = Common.encode_vector(catches, &encode_catch(&1, ctx, labels))

    do_collect_instructions(
      remaining,
      ctx,
      [{:instr, "try_table", [{:int, blocktype}, {:binary, catch_encoded}], labels} | acc],
      new_labels
    )
  end

  defp do_collect_instructions([[{:keyword, "try"} | args] | rest], ctx, acc, labels) do
    # try blocks (from Exception Handling) have a complex try/do/catch structure
    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        other -> {nil, other}
      end

    new_labels = [label | labels]

    blocktype_list = encode_control_flow_immediates(args, ctx)

    blocktype =
      case blocktype_list do
        [bt] -> bt
        list -> hd(list)
      end

    inner_rest =
      Enum.drop_while(args, fn
        [{:keyword, k} | _] when is_signature_definition(k) -> true
        _ -> false
      end)

    inner_instrs = collect_try_instrs(inner_rest, ctx, new_labels)

    acc = [{:instr, "try", [{:int, blocktype}], labels} | acc]
    acc = Enum.reduce(inner_instrs, acc, fn instr, a -> [instr | a] end)
    new_acc = [{:instr, "end", [], new_labels} | acc]

    do_collect_instructions(rest, ctx, new_acc, labels)
  end

  defp do_collect_instructions([[{:keyword, name} | args] | rest], ctx, acc, labels)
       when is_control_flow(name) do
    # Folded-style control flow: (block $id (result i32) ...)
    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        other -> {nil, other}
      end

    new_labels = [label | labels]

    blocktype_list = encode_control_flow_immediates(args, ctx)

    blocktype =
      case blocktype_list do
        [bt] -> bt
        list -> hd(list)
      end

    inner_rest =
      Enum.drop_while(args, fn
        [{:keyword, k} | _] when is_signature_definition(k) -> true
        _ -> false
      end)

    {inner_instrs, cond_instrs} =
      case name do
        "if" ->
          # if blocks have an optional condition sub-block when folded
          {collect_if_instrs(inner_rest, ctx, new_labels),
           collect_if_condition(inner_rest, ctx, labels)}

        _ ->
          {collect_instructions(inner_rest, ctx, new_labels), []}
      end

    acc = Enum.reduce(cond_instrs, acc, fn instr, a -> [instr | a] end)
    acc = [{:instr, name, [{:int, blocktype}], labels} | acc]
    acc = Enum.reduce(inner_instrs, acc, fn instr, a -> [instr | a] end)
    new_acc = [{:instr, "end", [], new_labels} | acc]

    do_collect_instructions(rest, ctx, new_acc, labels)
  end

  defp do_collect_instructions([{:keyword, name} | rest], ctx, acc, labels)
       when is_control_flow(name) do
    # Flat-style control flow: block $id ... end
    {args, remaining} = collect_args(rest, [])

    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        other -> {nil, other}
      end

    new_labels = [label | labels]

    blocktype_list = encode_control_flow_immediates(args, ctx)

    blocktype =
      case blocktype_list do
        [bt] -> bt
        list -> hd(list)
      end

    do_collect_instructions(
      remaining,
      ctx,
      [{:instr, name, [{:int, blocktype}], labels} | acc],
      new_labels
    )
  end

  defp do_collect_instructions([{:keyword, "end"} | rest], ctx, acc, labels) do
    new_labels =
      case labels do
        [_ | tail] -> tail
        [] -> []
      end

    do_collect_instructions(rest, ctx, [{:instr, "end", [], labels} | acc], new_labels)
  end

  defp do_collect_instructions([[{:keyword, name} | _] | rest], ctx, acc, labels)
       when is_ignored_instruction_node(name) do
    do_collect_instructions(rest, ctx, acc, labels)
  end

  defp do_collect_instructions([[{:keyword, "result"} | _] | rest], ctx, acc, labels) do
    do_collect_instructions(rest, ctx, acc, labels)
  end

  defp do_collect_instructions([{:keyword, name} | rest], ctx, acc, labels) do
    # Standard flat instruction: i32.add
    {args, remaining} = collect_args(rest, [])
    do_collect_instructions(remaining, ctx, [{:instr, name, args, labels} | acc], labels)
  end

  defp do_collect_instructions([[{:keyword, name} | args] | rest], ctx, acc, labels) do
    # Folded instruction: (i32.add (i32.const 1) (i32.const 2))
    # Immediates are the non-instruction arguments (ids, ints, or specific metadata keywords)
    immediates =
      Enum.filter(args, fn
        item when is_list(item) ->
          # Metadata blocks like (memory $m) or (type $t) are immediates,
          # but nested instructions like (i32.const 1) are not.
          case item do
            [{:keyword, k} | _] when k in ["memory", "type", "table", "result", "param"] -> true
            _ -> false
          end

        _other ->
          true
      end)

    new_acc = Enum.reduce(collect_folded_args(args, ctx, labels), acc, fn i, a -> [i | a] end)
    new_acc = [{:instr, name, immediates, labels} | new_acc]

    do_collect_instructions(rest, ctx, new_acc, labels)
  end

  defp do_collect_instructions([_ | rest], ctx, acc, labels),
    do: do_collect_instructions(rest, ctx, acc, labels)

  defp collect_if_condition(inner_rest, ctx, labels),
    do:
      Enum.reject(inner_rest, fn
        [{:keyword, k} | _] when is_if_branch_metadata(k) -> true
        _ -> false
      end)
      |> collect_instructions(ctx, labels)

  defp collect_if_instrs(inner_rest, ctx, labels) do
    then_body =
      inner_rest
      |> Enum.find(&match?([{:keyword, "then"} | _], &1))
      |> case do
        [{:keyword, "then"} | body] -> collect_instructions(body, ctx, labels)
        _ -> []
      end

    else_clause = Enum.find(inner_rest, &match?([{:keyword, "else"} | _], &1))

    else_body =
      case else_clause do
        [{:keyword, "else"} | body] ->
          else_instrs = collect_instructions(body, ctx, labels)

          # Only emit else instruction if there are instructions in the else body
          case else_instrs do
            [] -> []
            _ -> [{:instr, "else", [], labels} | else_instrs]
          end

        _ ->
          []
      end

    then_body ++ else_body
  end

  defp collect_try_instrs(inner_rest, ctx, labels) do
    do_body =
      inner_rest
      |> Enum.find(&match?([{:keyword, "do"} | _], &1))
      |> case do
        [{:keyword, "do"} | body] -> collect_instructions(body, ctx, labels)
        _ -> []
      end

    catches =
      Enum.flat_map(inner_rest, fn
        [{:keyword, "catch"}, tag_idx | body] ->
          [{:instr, "catch", [tag_idx], labels} | collect_instructions(body, ctx, labels)]

        [{:keyword, "catch_all"} | body] ->
          [{:instr, "catch_all", [], labels} | collect_instructions(body, ctx, labels)]

        _ ->
          []
      end)

    do_body ++ catches
  end

  defp collect_folded_args(args, ctx, labels),
    do:
      Enum.filter(args, &match?([{:keyword, _} | _], &1))
      |> Enum.flat_map(&collect_instructions([&1], ctx, labels))

  defp encode_catch([{:keyword, "catch"}, tag_id, label_id], ctx, labels) do
    [
      0x00,
      Common.encode_u32(resolve_index(extract_id(tag_id), ctx.tags, ctx.imports, "tag")),
      Common.encode_u32(resolve_label(extract_id(label_id), labels))
    ]
  end

  defp encode_catch([{:keyword, "catch_ref"}, tag_id, label_id], ctx, labels) do
    [
      0x01,
      Common.encode_u32(resolve_index(extract_id(tag_id), ctx.tags, ctx.imports, "tag")),
      Common.encode_u32(resolve_label(extract_id(label_id), labels))
    ]
  end

  defp encode_catch([{:keyword, "catch_all"}, label_id], _ctx, labels) do
    [
      0x02,
      Common.encode_u32(resolve_label(extract_id(label_id), labels))
    ]
  end

  defp encode_catch([{:keyword, "catch_all_ref"}, label_id], _ctx, labels) do
    [
      0x03,
      Common.encode_u32(resolve_label(extract_id(label_id), labels))
    ]
  end

  defp extract_id({:id, id}), do: id
  defp extract_id({:int, i}), do: i
  defp extract_id(id) when is_binary(id), do: id
end
