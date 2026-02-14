defmodule Watusi.Encoder.Instructions do
  @moduledoc """
  Logic for encoding WASM instructions and their immediates.
  """
  alias Watusi.Encoder.Common
  alias Watusi.Encoder.Sections
  alias Watusi.Instructions
  import Bitwise

  @control_flow_ops ["block", "loop", "if", "try"]
  @branch_ops ["br", "br_if", "br_table"]
  @call_ops ["call", "call_indirect", "return_call", "return_call_indirect"]
  @global_ops ["global.get", "global.set"]
  @tag_ops ["throw", "rethrow"]

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
    "v128.store",
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
    "v128.store",
    "v128.const",
    "i8x16.swizzle",
    "i32x4.splat",
    "f32x4.splat",
    "i32x4.extract_lane",
    "i32x4.replace_lane",
    "i32x4.eq",
    "i32x4.lt_s",
    "i32x4.lt_u",
    "f32x4.lt",
    "v128.and",
    "v128.or",
    "v128.bitselect",
    "v128.any_true",
    "i32x4.neg",
    "i32x4.shl",
    "i32x4.add",
    "i32x4.min_s",
    "f32x4.add",
    "f32x4.sub",
    "f32x4.mul",
    "f32x4.div",
    "f32x4.convert_i32x4_u",
    "f32x4.relaxed_min"
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

  @table_ops ["table.get", "table.set"]

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

  @immediate_types [:int, :float, :id, :offset, :align]

  # Guards for instruction categories
  defguard is_control_flow(name) when name in @control_flow_ops
  defguard is_branch(name) when name in @branch_ops
  defguard is_call(name) when name in @call_ops
  defguard is_global_op(name) when name in @global_ops
  defguard is_bulk_mem(name) when name in @bulk_mem_ops
  defguard is_mem_instr(name) when name in @memory_ops
  defguard is_simd(name) when name in @simd_ops
  defguard is_atomic(name) when name in @atomic_ops
  defguard is_table_op(name) when name in @table_ops
  defguard is_tag_op(name) when name in @tag_ops
  defguard is_gc_op(name) when name in @gc_ops

  def encode_instruction({:instr, name, args, labels}, ctx) do
    case Instructions.opcode(name) do
      {:fc, op} ->
        [0xFC, Common.encode_u32(op) | encode_immediates(name, args, ctx, labels)]

      {:fd, op} ->
        [0xFD, Common.encode_u32(op) | encode_immediates(name, args, ctx, labels)]

      {:fe, op} ->
        [0xFE, Common.encode_u32(op) | encode_immediates(name, args, ctx, labels)]

      {:fb, op} ->
        [0xFB, Common.encode_u32(op) | encode_immediates(name, args, ctx, labels)]

      opcode ->
        [opcode | encode_immediates(name, args, ctx, labels)]
    end
  end

  defp encode_immediates(name, args, ctx, _labels) when is_atomic(name) do
    # Atomic operations are specialized memory operations that sometimes
    # require a 'dummy' memory index (currently always 0 in WASM Core 1.0)
    encode_atomic_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_mem_instr(name) do
    # Standard memory operations (load/store) have alignment and offset immediates
    encode_mem_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_control_flow(name) do
    # block/loop/if/try require a blocktype immediate
    encode_control_flow_immediates(args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_bulk_mem(name) do
    # Bulk memory instructions (copy/fill/init) have various index immediates
    encode_bulk_mem_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_simd(name) do
    # SIMD instructions have specialized immediates (lanes, constants)
    encode_simd_immediates(name, args, ctx)
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

  defp encode_immediates(name, _args, _ctx, _labels)
       when name in ["memory.grow", "memory.size"] do
    # These always target memory 0 in Core 1.0
    [0x00]
  end

  defp encode_immediates(name, args, ctx, labels)
       when is_branch(name) or is_call(name) or name == "br_table" or name == "catch" do
    # These target labels, functions, or tags
    encode_dynamic_immediates(name, args, ctx, labels)
  end

  defp encode_immediates(name, args, ctx, _labels) do
    # Catch-all for instructions with simple immediates
    Enum.map(args, &encode_arg(name, &1, ctx, []))
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
    # call_indirect needs the type index and the table index (currently always 0)
    type_idx =
      case Enum.find(args, &match?([{:keyword, "type"}, _], &1)) do
        [{:keyword, "type"}, {:id, id}] -> resolve_type_id(id, ctx)
        [{:keyword, "type"}, {:int, i} | _] -> i
        _ -> 0
      end

    [Common.encode_u32(type_idx), Common.encode_u32(0)]
  end

  defp encode_dynamic_immediates("return_call_indirect", args, ctx, _labels) do
    # tail-call variant of call_indirect
    type_idx =
      case Enum.find(args, &match?([{:keyword, "type"}, _], &1)) do
        [{:keyword, "type"}, {:id, id}] -> resolve_type_id(id, ctx)
        [{:keyword, "type"}, {:int, i} | _] -> i
        _ -> 0
      end

    [Common.encode_u32(type_idx), Common.encode_u32(0)]
  end

  defp encode_dynamic_immediates(name, args, ctx, labels)
       when name in ["br", "br_if", "call", "return_call", "catch"] do
    Enum.map(args, &encode_arg(name, &1, ctx, labels))
  end

  defp encode_control_flow_immediates(args, ctx) do
    # Block types can be void (0x40), a single valtype, or a type index (signed LEB)
    case args do
      [{:int, bt}] ->
        [bt]

      _ ->
        results =
          args
          |> Enum.filter(&match?([{:keyword, "result"} | _], &1))
          |> Enum.flat_map(fn [{:keyword, "result"} | ts] -> ts end)
          |> Enum.map(fn {:keyword, type} -> type end)

        case results do
          [] ->
            [0x40]

          [type] ->
            [Instructions.valtype(type)]

          types ->
            # Multi-value blocks must reference a signature in the Type section
            sig = {[], types}

            idx =
              Enum.find_index(ctx.sigs, &(&1 == sig)) ||
                raise("Type not found for block: #{inspect(sig)}")

            [LEB128.encode_signed(idx)]
        end
    end
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
        {:id, id} -> resolve_type_id(id, ctx)
        _ ->
          case Enum.find(args, &match?({:int, _}, &1)) do
            {:int, i} -> i
            _ -> 0
          end
      end

    # 2. Resolve field index (second immediate for struct.get/set)
    case name do
      n when n in ["struct.get", "struct.get_s", "struct.get_u", "struct.set"] ->
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

  defp encode_simd_immediates(name, args, ctx) when name in ["v128.load", "v128.store"] do
    encode_mem_immediates(name, args, ctx)
  end

  defp encode_simd_immediates(name, args, _ctx) when name in ["i32x4.extract_lane", "i32x4.replace_lane"] do
    case Enum.find(args, &match?({:int, _}, &1)) do
      {:int, lane} -> [lane]
      _ -> [0]
    end
  end

  defp encode_simd_immediates("v128.const", args, _ctx) do
    # v128.const can be initialized from various shapes (i8x16, i32x4, etc.)
    values =
      args
      |> Enum.filter(fn
        {:int, _} -> true
        {:float, _} -> true
        _ -> false
      end)

    case values do
      vals when length(vals) == 16 -> Enum.map(vals, fn {:int, v} -> <<v::8>> end)
      vals when length(vals) == 8 -> Enum.map(vals, fn {:int, v} -> <<v::little-16>> end)
      vals when length(vals) == 4 ->
        Enum.map(vals, fn
          {:int, v} -> <<v::little-32>>
          {:float, v} -> <<v::float-little-32>>
        end)
      vals when length(vals) == 2 ->
        Enum.map(vals, fn
          {:int, v} -> <<v::little-64>>
          {:float, v} -> <<v::float-little-64>>
        end)
      other -> raise "Invalid v128.const arguments: #{inspect(other)}"
    end
    |> IO.iodata_to_binary()
    |> List.wrap()
  end

  defp encode_simd_immediates(_name, _args, _ctx), do: []

  defp encode_bulk_mem_immediates("memory.init", args, ctx) do
    dataidx =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_data_index(id, ctx.data)
        {:int, i} -> i
      end

    [Common.encode_u32(dataidx), 0x00]
  end

  defp encode_bulk_mem_immediates("data.drop", args, ctx) do
    dataidx =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_data_index(id, ctx.data)
        {:int, i} -> i
      end

    [Common.encode_u32(dataidx)]
  end

  defp encode_bulk_mem_immediates("memory.copy", _, _), do: [0x00, 0x00]
  defp encode_bulk_mem_immediates("memory.fill", _, _), do: [0x00]

  defp encode_bulk_mem_immediates("table.init", args, ctx) do
    elemidx =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_elem_index(id, ctx.elems)
        {:int, i} -> i
      end

    tableidx =
      case Enum.at(args, 1) do
        {:id, id} -> resolve_index(id, ctx.tables, ctx.imports, "table")
        {:int, i} -> i
        nil -> 0
      end

    [Common.encode_u32(elemidx), Common.encode_u32(tableidx)]
  end

  defp encode_bulk_mem_immediates("elem.drop", args, ctx) do
    elemidx =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_elem_index(id, ctx.elems)
        {:int, i} -> i
      end

    [Common.encode_u32(elemidx)]
  end

  defp encode_bulk_mem_immediates("table.copy", args, ctx) do
    dst =
      case Enum.at(args, 0) do
        {:id, id} -> resolve_index(id, ctx.tables, ctx.imports, "table")
        {:int, i} -> i
        nil -> 0
      end

    src =
      case Enum.at(args, 1) do
        {:id, id} -> resolve_index(id, ctx.tables, ctx.imports, "table")
        {:int, i} -> i
        nil -> 0
      end

    [Common.encode_u32(dst), Common.encode_u32(src)]
  end

  defp encode_mem_immediates(name, args, ctx) do
    # 1. Resolve memory index. Defaults to 0 (the primary memory).
    # In Multi-Memory, an explicit (memory $idx) can be provided.
    mem_node = Enum.find(args, &match?([{:keyword, "memory"} | _], &1))

    mem_idx =
      case mem_node do
        [{:keyword, "memory"}, {:id, id}] -> resolve_index(id, ctx.memories, ctx.imports, "memory")
        [{:keyword, "memory"}, {:int, i}] -> i
        _ -> 0
      end

    # 2. Determine if the targeted memory is 64-bit.
    # Memory64 uses 64-bit offsets in the binary encoding.
    is_64? = memory_is_64?(mem_idx, ctx)

    offset =
      case Enum.find(args, &match?({:offset, _}, &1)) do
        {:offset, v} -> v
        _ -> 0
      end

    align =
      case Enum.find(args, &match?({:align, _}, &1)) do
        {:align, v} -> to_log2(v)
        _ -> natural_align(name)
      end

    # 3. Encode flags (alignment + extension bits).
    # Bit 6 (0x40) indicates that a memory index follows.
    flags = if mem_idx > 0, do: align ||| 0x40, else: align

    encoded_offset =
      if is_64?, do: LEB128.encode_unsigned(offset), else: Common.encode_u32(offset)

    if mem_idx > 0 do
      [Common.encode_u32(flags), Common.encode_u32(mem_idx), encoded_offset]
    else
      [Common.encode_u32(flags), encoded_offset]
    end
  end

  defp memory_is_64?(idx, ctx) do
    # We find the memory definition at the given index (accounting for imports).
    import_count =
      Enum.count(ctx.imports, fn item ->
        case Sections.normalize_import(item) do
          {_, _, "memory", _} -> true
          _ -> false
        end
      end)

    case idx < import_count do
      true ->
        # Check the import definition
        import_item =
          ctx.imports
          |> Enum.filter(fn item ->
            case Sections.normalize_import(item) do
              {_, _, "memory", _} -> true
              _ -> false
            end
          end)
          |> Enum.at(idx)

        {_, _, _, rest} = Sections.normalize_import(import_item)
        Enum.any?(rest, &match?({:keyword, "i64"}, &1))

      false ->
        # Check the local definition
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

  defp encode_arg(name, {:id, id}, _, labels) when is_branch(name),
    do: resolve_label(id, labels) |> Common.encode_u32()

  defp encode_arg("i32.const", {:int, val}, _, _), do: val |> wrap_i32() |> LEB128.encode_signed()
  defp encode_arg("i64.const", {:int, val}, _, _), do: val |> wrap_i64() |> LEB128.encode_signed()

  defp encode_arg("f32.const", {:float, :neg_nan}, _, _), do: <<0, 0, 192, 255>>

  defp encode_arg("f32.const", {:float, {:nan, sign, payload}}, _, _) do
    bits = encode_sign(sign) <<< 31 ||| 0xFF <<< 23 ||| (payload &&& 0x7FFFFF)
    <<bits::little-32>>
  end

  defp encode_arg("f32.const", {:float, val}, _, _), do: encode_f32(val)
  defp encode_arg("f32.const", {:int, val}, _, _), do: encode_f32(val * 1.0)

  defp encode_arg("f64.const", {:float, :neg_nan}, _, _), do: <<0, 0, 0, 0, 0, 0, 248, 255>>

  defp encode_arg("f64.const", {:float, {:nan, sign, payload}}, _, _) do
    bits = encode_sign(sign) <<< 63 ||| 0x7FF <<< 52 ||| (payload &&& 0xFFFFFFFFFFFFF)
    <<bits::little-64>>
  end

  defp encode_arg("f64.const", {:float, val}, _, _), do: encode_f64(val)
  defp encode_arg("f64.const", {:int, val}, _, _), do: encode_f64(val * 1.0)

  defp encode_arg(name, {:id, id}, ctx, _labels)
       when name in ["call", "return_call", "throw", "catch"],
       do: Common.encode_u32(resolve_index(id, get_list_for_kind(name, ctx), ctx.imports, get_kind_for_op(name)))

  defp encode_arg(name, {:id, id}, ctx, _) when is_global_op(name),
    do: Common.encode_u32(resolve_index(id, ctx.globals, ctx.imports, "global"))

  defp encode_arg(_name, {:int, val}, _, _), do: Common.encode_u32(val)
  defp encode_arg(_name, {:id, id}, ctx, _), do: Common.encode_u32(Map.fetch!(ctx.local_map, id))

  defp get_list_for_kind(name, ctx) when name in ["call", "return_call"], do: ctx.funcs
  defp get_list_for_kind(name, ctx) when name in ["throw", "catch"], do: ctx.tags

  defp get_kind_for_op(name) when name in ["call", "return_call"], do: "func"
  defp get_kind_for_op(name) when name in ["throw", "catch"], do: "tag"

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

  defp resolve_label(id, labels) do
    # Labels are resolved to their relative depth in the label stack
    Enum.find_index(labels, &(&1 == id)) || raise("Label not found: $#{id}")
  end

  defp resolve_type_id(id, ctx) do
    # Multi-value blocks and call_indirect need to reference a signature in the Type section
    idx =
      Enum.find_index(ctx.types, &match?([{:keyword, "type"}, {:id, ^id} | _], &1)) ||
        raise("Type not found: $#{id}")

    sig = Sections.extract_raw_signature(Enum.at(ctx.types, idx))
    Enum.find_index(ctx.sigs, &(&1 == sig))
  end

  defp encode_f32(:infinity), do: <<0, 0, 128, 127>>
  defp encode_f32(:neg_infinity), do: <<0, 0, 128, 255>>
  defp encode_f32(:nan), do: <<0, 0, 192, 127>>
  defp encode_f32(f), do: <<f::float-little-size(32)>>

  defp encode_f64(:infinity), do: <<0, 0, 0, 0, 0, 0, 240, 127>>
  defp encode_f64(:neg_infinity), do: <<0, 0, 0, 0, 0, 0, 240, 255>>
  defp encode_f64(:nan), do: <<0, 0, 0, 0, 0, 0, 248, 127>>
  defp encode_f64(f), do: <<f::float-little-size(64)>>

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

  defp resolve_data_index(id, data),
    do:
      Enum.find_index(data, &match?([{:keyword, "data"}, {:id, ^id} | _], &1)) ||
        raise("Data ID not found: $#{id}")

  defp resolve_elem_index(id, elems),
    do:
      Enum.find_index(elems, &match?([{:keyword, "elem"}, {:id, ^id} | _], &1)) ||
        raise("Elem ID not found: $#{id}")

  @simd_shapes ["i8x16", "i16x8", "i32x4", "i64x2", "f32x4", "f64x2"]

  def collect_args([{type, _} = arg | rest], acc) when type in @immediate_types,
    do: collect_args(rest, [arg | acc])

  def collect_args([{:keyword, shape} = arg | rest], acc) when shape in @simd_shapes,
    do: collect_args(rest, [arg | acc])

  def collect_args([[{:keyword, "result"} | _] = arg | rest], acc),
    do: collect_args(rest, [arg | acc])

  def collect_args([[{:keyword, name} | _] = arg | rest], acc) when name in ["memory", "type"],
    do: collect_args(rest, [arg | acc])

  def collect_args(rest, acc), do: {Enum.reverse(acc), rest}

  def build_local_map([{:keyword, "func"} | rest]) do
    # Locals (params and local declarations) are indexed consecutively
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    {_, map} =
      Enum.reduce(rest, {0, %{}}, fn
        [{:keyword, kind} | ts], {idx, acc} when kind in ["param", "local"] ->
          Enum.reduce(ts, {idx, acc}, fn
            {:id, id}, {i, a} -> {i, Map.put(a, id, i)}
            {:keyword, _}, {i, a} -> {i + 1, a}
            _, state -> state
          end)

        _, state ->
          state
      end)

    map
  end

  @rejected_metadata_kinds ["param", "local"]
  defguardp is_rejected_metadata(kind) when kind in @rejected_metadata_kinds

  @metadata_kinds ["type", "param", "local", "export", "then", "else", "try", "catch", "delegate", "catch_all", "do", "memory"]
  defguardp is_metadata(kind) when kind in @metadata_kinds

  def collect_instructions(rest, ctx, label_stack \\ []) do
    # Instructions are collected into a flat list, resolving identifiers
    # and nesting into a linear sequence of WASM opcodes.
    rest =
      case rest do
        [{:id, _} | tail] -> tail
        other -> other
      end

    rest
    |> Enum.reject(fn
      [{:keyword, "export"}, _] -> true
      [{:keyword, k} | _] when is_rejected_metadata(k) -> true
      _ -> false
    end)
    |> do_collect_instructions(ctx, [], label_stack)
  end

  defp do_collect_instructions([], _ctx, acc, _labels), do: Enum.reverse(acc)

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

    inner_rest = Enum.reject(args, &match?([{:keyword, "result"} | _], &1))
    inner_instrs = collect_try_instrs(inner_rest, ctx, new_labels)

    new_acc =
      [{:instr, "end", [], labels}] ++
        Enum.reverse(inner_instrs) ++
        [{:instr, "try", [{:int, blocktype}], labels}] ++ acc

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
      Enum.reject(args, fn
        [{:keyword, "result"} | _] -> true
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

    new_acc =
      [{:instr, "end", [], labels}] ++
        Enum.reverse(inner_instrs) ++
        [{:instr, name, [{:int, blocktype}], labels}] ++ Enum.reverse(cond_instrs) ++ acc

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

    do_collect_instructions(remaining, ctx, [{:instr, name, [{:int, blocktype}], labels} | acc], new_labels)
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
       when is_metadata(name) do
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
            [{:keyword, k} | _] when k in ["memory", "type", "result"] -> true
            _ -> false
          end

        _other ->
          true
      end)

    new_acc =
      [
        {:instr, name, immediates, labels}
        | Enum.reverse(collect_folded_args(args, ctx, labels))
      ] ++ acc

    do_collect_instructions(rest, ctx, new_acc, labels)
  end

  defp do_collect_instructions([_ | rest], ctx, acc, labels),
    do: do_collect_instructions(rest, ctx, acc, labels)

  defp collect_if_condition(inner_rest, ctx, labels),
    do:
      Enum.reject(inner_rest, fn
        [{:keyword, k} | _] when k in ["then", "else"] -> true
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

    else_body =
      inner_rest
      |> Enum.find(&match?([{:keyword, "else"} | _], &1))
      |> case do
        [{:keyword, "else"} | body] ->
          [{:instr, "else", [], labels} | collect_instructions(body, ctx, labels)]

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
      inner_rest
      |> Enum.filter(&match?([{:keyword, k} | _] when k in ["catch", "catch_all"], &1))
      |> Enum.flat_map(fn
        [{:keyword, "catch"}, tag_idx | body] ->
          [{:instr, "catch", [tag_idx], labels} | collect_instructions(body, ctx, labels)]

        [{:keyword, "catch_all"} | body] ->
          [{:instr, "catch_all", [], labels} | collect_instructions(body, ctx, labels)]
      end)

    do_body ++ catches
  end

  defp collect_folded_args(args, ctx, labels),
    do:
      Enum.filter(args, &match?([{:keyword, _} | _], &1))
      |> Enum.flat_map(&collect_instructions([&1], ctx, labels))
end
