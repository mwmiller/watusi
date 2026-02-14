defmodule Watusi.ParserTest do
  use ExUnit.Case
  alias Watusi.Lexer
  alias Watusi.Parser

  test "parses tokens into S-expressions" do
    input = "(module (func (param i32) (result i32)))"
    tokens = Lexer.tokenize(input)
    result = Parser.parse(tokens)

    assert result == [
             [
               {:keyword, "module"},
               [
                 {:keyword, "func"},
                 [{:keyword, "param"}, {:keyword, "i32"}],
                 [{:keyword, "result"}, {:keyword, "i32"}]
               ]
             ]
           ]
  end
end
