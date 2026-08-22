# Phase and micro-benchmarks for the WAT -> WASM pipeline.
#
# Usage: mix run scripts/bench.exs [phases|internals]

inputs = %{
  "skip-stack 160KB" => File.read!("test/spec_vectors/skip-stack-guard-page/ok/module_0.wat"),
  "br_table 52KB" => File.read!("test/spec_vectors/br_table/ok/module_0.wat"),
  "names 31KB" => File.read!("test/spec_vectors/names/ok/module_2.wat")
}

prepared =
  Map.new(inputs, fn {name, wat} ->
    tokens = Watusi.Lexer.tokenize(wat)
    ast = Watusi.Parser.parse(tokens)
    {name, %{wat: wat, tokens: tokens, ast: ast}}
  end)

case System.get_env("BENCH_MODE", "phases") do
  "phases" ->
    Benchee.run(
      %{
        "tokenize" => fn p -> Watusi.Lexer.tokenize(p.wat) end,
        "parse" => fn p -> Watusi.Parser.parse(p.tokens) end,
        "encode" => fn p -> Watusi.Encoder.encode(p.ast) end,
        "full to_wasm" => fn p -> Watusi.to_wasm(p.wat) end
      },
      inputs: prepared,
      time: 3,
      warmup: 1,
      print: [comparison: false]
    )

  "internals" ->
    Benchee.run(
      %{
        "group_sections" => fn p ->
          [node | _] = p.ast
          [_module | body] = node
          Watusi.Encoder.Sections.group_sections(body)
        end,
        "prepare_signatures" => fn p ->
          [node | _] = p.ast
          body = Enum.drop(node, 1)
          sections = Watusi.Encoder.Sections.group_sections(body)
          Watusi.Encoder.Sections.prepare_signatures(sections)
        end,
        "encode_u32 hot" => fn _p ->
          Enum.each(1..20_000, fn i ->
            Watusi.Encoder.Common.encode_u32(rem(i * 7919, 4_294_967_295))
          end)
        end
      },
      inputs: prepared,
      time: 3,
      warmup: 1,
      print: [comparison: false]
    )
end
