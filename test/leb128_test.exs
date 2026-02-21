defmodule Watusi.LEB128Test do
  use ExUnit.Case
  alias Watusi.LEB128

  describe "encode_unsigned/1" do
    test "encodes small values" do
      assert LEB128.encode_unsigned(0) == <<0>>
      assert LEB128.encode_unsigned(1) == <<1>>
      assert LEB128.encode_unsigned(127) == <<127>>
    end

    test "encodes values requiring multiple bytes" do
      assert LEB128.encode_unsigned(128) == <<0x80, 0x01>>
      assert LEB128.encode_unsigned(624_485) == <<0xE5, 0x8E, 0x26>>
      assert LEB128.encode_unsigned(16_384) == <<0x80, 0x80, 0x01>>
    end

    test "encodes large values" do
      assert LEB128.encode_unsigned(0xFFFFFFFF) == <<0xFF, 0xFF, 0xFF, 0xFF, 0x0F>>
    end
  end

  describe "encode_signed/1" do
    test "encodes small positive values" do
      assert LEB128.encode_signed(0) == <<0>>
      assert LEB128.encode_signed(1) == <<1>>
      assert LEB128.encode_signed(63) == <<63>>
    end

    test "encodes small negative values" do
      assert LEB128.encode_signed(-1) == <<0x7F>>
      assert LEB128.encode_signed(-64) == <<0x40>>
    end

    test "encodes larger values" do
      assert LEB128.encode_signed(-123_456) == <<0xC0, 0xBB, 0x78>>
      assert LEB128.encode_signed(123_456) == <<0xC0, 0xC4, 0x07>>
    end

    test "encodes boundary values" do
      assert LEB128.encode_signed(64) == <<0xC0, 0x00>>
      assert LEB128.encode_signed(-65) == <<0xBF, 0x7F>>
    end
  end
end
