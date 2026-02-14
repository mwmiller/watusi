defmodule Watusi.LexerTest do
  use ExUnit.Case
  alias Watusi.Lexer

  test "tokenizes basic module" do
    input =
      "(module (func $add (param i32 i32) (result i32) (i32.add (local.get 0) (local.get 1))))"

    tokens = Lexer.tokenize(input)

    assert tokens == [
             :lparen,
             {:keyword, "module"},
             :lparen,
             {:keyword, "func"},
             {:id, "add"},
             :lparen,
             {:keyword, "param"},
             {:keyword, "i32"},
             {:keyword, "i32"},
             :rparen,
             :lparen,
             {:keyword, "result"},
             {:keyword, "i32"},
             :rparen,
             :lparen,
             {:keyword, "i32.add"},
             :lparen,
             {:keyword, "local.get"},
             {:int, 0},
             :rparen,
             :lparen,
             {:keyword, "local.get"},
             {:int, 1},
             :rparen,
             :rparen,
             :rparen,
             :rparen
           ]
  end

  test "handles comments" do
    input = """
    ;; line comment
    (module
      (; block
         comment ;)
      (func)
    )
    """

    tokens = Lexer.tokenize(input)

    assert tokens == [
             :lparen,
             {:keyword, "module"},
             :lparen,
             {:keyword, "func"},
             :rparen,
             :rparen
           ]
  end

  test "handles strings" do
    input = ~s{(export "main" (func $main))}
    tokens = Lexer.tokenize(input)

    assert tokens == [
             :lparen,
             {:keyword, "export"},
             {:string, "main"},
             :lparen,
             {:keyword, "func"},
             {:id, "main"},
             :rparen,
             :rparen
           ]
  end
end
