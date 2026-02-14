defmodule Watusi.Encoder.Instructions do
  @moduledoc """
  Logic for encoding WASM instructions and their immediates.
  """
  alias Watusi.Encoder.Common
  alias Watusi.Encoder.Sections
  alias Watusi.Instructions
  import Bitwise

  @control_flow_ops ["block", "loop", "if"]
  @branch_ops ["br", "br_if", "br_table"]
  @call_ops ["call", "call_indirect"]
  @global_ops ["global.get", "global.set"]

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
    "i32x4.extract_lane",
    "i32x4.eq",
    "i32x4.lt_s",
    "v128.bitselect",
    "i32x4.neg",
    "i32x4.add",
    "i32x4.min_s",
    "f32x4.add"
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

  def encode_instruction({:instr, name, args, labels}, ctx) do
    case Instructions.opcode(name) do
      {:fc, op} ->
        [0xFC, Common.encode_u32(op) | encode_immediates(name, args, ctx, labels)]

      {:fd, op} ->
        [0xFD, Common.encode_u32(op) | encode_immediates(name, args, ctx, labels)]

      {:fe, op} ->
        [0xFE, Common.encode_u32(op) | encode_immediates(name, args, ctx, labels)]

      opcode ->
        [opcode | encode_immediates(name, args, ctx, labels)]
    end
  end

  defp encode_immediates(name, args, _ctx, _labels) when is_mem_instr(name) do
    encode_mem_immediates(name, args)
  end

  defp encode_immediates(name, args, _ctx, _labels) when is_control_flow(name) do
    encode_control_flow_immediates(args)
  end

  defp encode_immediates(name, args, ctx, _labels) when is_bulk_mem(name) do
    encode_bulk_mem_immediates(name, args, ctx)
  end

  defp encode_immediates(name, args, _ctx, _labels) when is_simd(name) do
    encode_simd_immediates(name, args)
  end

  defp encode_immediates(name, args, _ctx, _labels) when is_atomic(name) do
    encode_atomic_immediates(name, args)
  end

  defp encode_immediates(name, _args, _ctx, _labels)
       when name in ["memory.grow", "memory.size"] do
    [0x00]
  end

  defp encode_immediates(name, args, ctx, labels)
       when is_branch(name) or is_call(name) or name == "br_table" do
    encode_dynamic_immediates(name, args, ctx, labels)
  end

  defp encode_immediates(name, args, ctx, _labels) do
    Enum.map(args, &encode_arg(name, &1, ctx, []))
  end

  defp encode_dynamic_immediates("br_table", args, _ctx, labels) do
    indices =
      Enum.map(args, fn
        {:id, id} -> resolve_label(id, labels)
        {:int, i} -> i
      end)

    case indices do
      [] ->
        raise "br_table needs target"

      _other ->
        {ts, [d]} = Enum.split(indices, -1)
        [Common.encode_vector(ts, &Common.encode_u32/1), Common.encode_u32(d)]
    end
  end

  defp encode_dynamic_immediates("call_indirect", args, ctx, _labels) do
    type_idx =
      case Enum.find(args, &match?([{:keyword, "type"}, _], &1)) do
        [{:keyword, "type"}, {:id, id}] -> resolve_type_id(id, ctx)
        [{:keyword, "type"}, {:int, i} | _other] -> i
        _other -> 0
      end

    [Common.encode_u32(type_idx), Common.encode_u32(0)]
  end

  defp encode_dynamic_immediates(name, args, ctx, labels) when is_branch(name) or is_call(name) do
    Enum.map(args, &encode_arg(name, &1, ctx, labels))
  end

  defp encode_control_flow_immediates(args) do
    case args do
      [{:int, bt} | _other] ->
        [bt]

      _other ->
        args
        |> Enum.find(&match?([{:keyword, "result"} | _other], &1))
        |> case do
          [{:keyword, "result"}, {:keyword, type}] -> [Instructions.valtype(type)]
          _other -> [0x40]
        end
    end
  end

  defp encode_atomic_immediates(name, args)
       when name in ["memory.atomic.notify", "memory.atomic.wait32", "memory.atomic.wait64"] do
    [0x00 | encode_mem_immediates(name, args)]
  end

  defp encode_atomic_immediates(name, args) do
    encode_mem_immediates(name, args)
  end

  defp encode_simd_immediates(name, args) when name in ["v128.load", "v128.store"] do
    encode_mem_immediates(name, args)
  end

  defp encode_simd_immediates(name, args) when name in ["i32x4.extract_lane"] do
    case Enum.find(args, &match?({:int, _}, &1)) do
      {:int, lane} -> [lane]
      _other -> [0]
    end
  end

  defp encode_simd_immediates("v128.const", args) do
    values =
      args
      |> Enum.filter(fn
        {:int, _} -> true
        {:float, _} -> true
        _other -> false
      end)

    case values do
      vals when length(vals) == 16 ->
        Enum.map(vals, fn {:int, v} -> <<v::8>> end)

      vals when length(vals) == 8 ->
        Enum.map(vals, fn {:int, v} -> <<v::little-16>> end)

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

      _other ->
        raise "Invalid v128.const arguments: #{inspect(args)}"
    end
    |> IO.iodata_to_binary()
    |> List.wrap()
  end

  defp encode_simd_immediates(_name, _args), do: []

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

  defp encode_mem_immediates(name, args) do
    offset =
      case Enum.find(args, &match?({:offset, _}, &1)) do
        {:offset, v} -> v
        _other -> 0
      end

    align =
      case Enum.find(args, &match?({:align, _}, &1)) do
        {:align, v} -> to_log2(v)
        _other -> natural_align(name)
      end

    [Common.encode_u32(align), Common.encode_u32(offset)]
  end

  defp natural_align(name) do
    cond do
      String.contains?(name, "v128") -> 4
      String.contains?(name, "8") -> 0
      String.contains?(name, "16") -> 1
      String.contains?(name, "64") -> 3
      String.contains?(name, "32") -> 2
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

  defp encode_arg("call", {:id, id}, ctx, _),
    do: Common.encode_u32(resolve_index(id, ctx.funcs, ctx.imports, "func"))

  defp encode_arg(name, {:id, id}, ctx, _) when is_global_op(name),
    do: Common.encode_u32(resolve_index(id, ctx.globals, ctx.imports, "global"))

  defp encode_arg(_name, {:int, val}, _, _), do: Common.encode_u32(val)
  defp encode_arg(_name, {:id, id}, ctx, _), do: Common.encode_u32(Map.fetch!(ctx.local_map, id))

  defp wrap_i32(val) do
    <<signed::signed-32>> = <<val::unsigned-32>>
    signed
  end

  defp wrap_i64(val) do
    <<signed::signed-64>> = <<val::unsigned-64>>
    signed
  end

  defp encode_sign(-1), do: 1
  defp encode_sign(_other), do: 0

  defp resolve_label(id, labels),
    do: Enum.find_index(labels, &(&1 == id)) || raise("Label not found: $#{id}")

  defp resolve_type_id(id, ctx) do
    idx =
      Enum.find_index(ctx.types, &match?([{:keyword, "type"}, {:id, ^id} | _other], &1)) ||
        raise("Type not found: $#{id}")

    # extract_raw_signature will be in Sections
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

  def resolve_index(id, list, imports, kind) do
    kind_imports =
      Enum.filter(imports, fn
        [{:keyword, "import"}, _, _, [{:keyword, ^kind} | _other]] -> true
        _other -> false
      end)

    case find_id_in_imports(kind_imports, kind, id) do
      nil ->
        length(kind_imports) +
          (find_local_index(list, kind, id) || raise("ID not found: #{kind} $#{id}"))

      idx ->
        idx
    end
  end

  defp find_id_in_imports(imports, kind, id) do
    Enum.find_index(imports, fn
      [{:keyword, "import"}, _, _, [{:keyword, ^kind}, {:id, ^id} | _other]] -> true
      _other -> false
    end)
  end

  defp find_local_index(ls, k, id),
    do: Enum.find_index(ls, &match?([{:keyword, ^k}, {:id, ^id} | _other], &1))

  defp resolve_data_index(id, data),
    do:
      Enum.find_index(data, &match?([{:keyword, "data"}, {:id, ^id} | _other], &1)) ||
        raise("Data ID not found: $#{id}")

  defp resolve_elem_index(id, elems),
    do:
      Enum.find_index(elems, &match?([{:keyword, "elem"}, {:id, ^id} | _other], &1)) ||
        raise("Elem ID not found: $#{id}")

  defp filter_immediates(args) do
    Enum.filter(args, fn
      {type, _val} -> type in @immediate_types
      _other -> false
    end)
  end

  def collect_args([{type, _} = arg | rest], acc) when type in @immediate_types,
    do: collect_args(rest, [arg | acc])

  def collect_args([[{:keyword, "result"} | _other] = arg | rest], acc),
    do: collect_args(rest, [arg | acc])

  def collect_args(rest, acc), do: {Enum.reverse(acc), rest}

  def build_local_map([{:keyword, "func"} | rest]) do
    rest =
      case rest do
        [{:id, _id} | tail] -> tail
        rest -> rest
      end

    {_final_idx, map} =
      Enum.reduce(rest, {0, %{}}, fn
        [{:keyword, kind} | ts], {idx, acc} when kind in ["param", "local"] ->
          {new_idx, new_acc} =
            Enum.reduce(ts, {idx, acc}, fn
              {:id, id}, {i, a} -> {i, Map.put(a, id, i)}
              {:keyword, _id}, {i, a} -> {i + 1, a}
              _other, state -> state
            end)

          {new_idx, new_acc}

        _other, state ->
          state
      end)

    map
  end

  def collect_instructions(rest, label_stack \\ []) do
    rest =
      case rest do
        [{:id, _id} | tail] -> tail
        rest -> rest
      end

    rest
    |> Enum.reject(fn
      [{:keyword, "export"}, _name] -> true
      [{:keyword, k} | _other] when k in ["param", "local"] -> true
      _other -> false
    end)
    |> do_collect_instructions([], label_stack)
  end

  defp do_collect_instructions([], acc, _labels), do: Enum.reverse(acc)

  defp do_collect_instructions([{:keyword, name} | rest], acc, labels)
       when is_control_flow(name) do
    # Flat Style control flow: (block $id (result i32) ...)
    {args, remaining} = collect_args(rest, [])

    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        _other -> {nil, args}
      end

    new_labels = [label | labels]

    do_collect_instructions(remaining, [{:instr, name, args, labels} | acc], new_labels)
  end

  defp do_collect_instructions([{:keyword, "end"} | rest], acc, labels) do
    # POP label
    new_labels =
      case labels do
        [_head | tail] -> tail
        [] -> []
      end

    do_collect_instructions(rest, [{:instr, "end", [], labels} | acc], new_labels)
  end

  defp do_collect_instructions([[{:keyword, name} | _other] | rest], acc, labels)
       when name in ["type", "param", "local", "export", "then", "else"] do
    do_collect_instructions(rest, acc, labels)
  end

  defp do_collect_instructions([[{:keyword, "result"} | _other] | rest], acc, labels) do
    # Result outside of control flow head is discarded (already handled in head)
    do_collect_instructions(rest, acc, labels)
  end

  defp do_collect_instructions([{:keyword, name} | rest], acc, labels) do
    {args, remaining} = collect_args(rest, [])
    do_collect_instructions(remaining, [{:instr, name, args, labels} | acc], labels)
  end

  defp do_collect_instructions([[{:keyword, name} | args] | rest], acc, labels)
       when is_control_flow(name) do
    # Folded Style control flow
    {label, args} =
      case args do
        [{:id, id} | tail] -> {id, tail}
        _other -> {nil, args}
      end

    new_labels = [label | labels]

    blocktype =
      case Enum.find(args, fn
             [{:keyword, "result"} | _other] -> true
             _other -> false
           end) do
        [{:keyword, "result"}, {:keyword, type}] -> Instructions.valtype(type)
        _other -> 0x40
      end

    inner_rest =
      Enum.reject(args, fn
        [{:keyword, "result"} | _other] -> true
        _other -> false
      end)

    {inner_instrs, cond_instrs} =
      case name do
        "if" ->
          {collect_if_instrs(inner_rest, new_labels), collect_if_condition(inner_rest, labels)}

        _other ->
          {collect_instructions(inner_rest, new_labels), []}
      end

    new_acc =
      [{:instr, "end", [], labels}] ++
        Enum.reverse(inner_instrs) ++
        [{:instr, name, [{:int, blocktype}], labels}] ++ Enum.reverse(cond_instrs) ++ acc

    do_collect_instructions(rest, new_acc, labels)
  end

  defp do_collect_instructions([[{:keyword, name} | args] | rest], acc, labels) do
    new_acc =
      [
        {:instr, name, filter_immediates(args), labels}
        | Enum.reverse(collect_folded_args(args, labels))
      ] ++ acc

    do_collect_instructions(rest, new_acc, labels)
  end

  defp do_collect_instructions([_other | rest], acc, labels),
    do: do_collect_instructions(rest, acc, labels)

  defp collect_if_condition(inner_rest, labels),
    do:
      Enum.reject(inner_rest, fn
        [{:keyword, k} | _other] when k in ["then", "else"] -> true
        _other -> false
      end)
      |> collect_instructions(labels)

  defp collect_if_instrs(inner_rest, labels) do
    then_body =
      inner_rest
      |> Enum.find(&match?([{:keyword, "then"} | _other], &1))
      |> case do
        [{:keyword, "then"} | body] -> collect_instructions(body, labels)
        _other -> []
      end

    else_body =
      inner_rest
      |> Enum.find(&match?([{:keyword, "else"} | _other], &1))
      |> case do
        [{:keyword, "else"} | body] ->
          [{:instr, "else", [], labels} | collect_instructions(body, labels)]

        _other ->
          []
      end

    then_body ++ else_body
  end

  defp collect_folded_args(args, labels),
    do:
      Enum.filter(args, &match?([{:keyword, _id} | _other], &1))
      |> Enum.flat_map(&collect_instructions([&1], labels))
end
