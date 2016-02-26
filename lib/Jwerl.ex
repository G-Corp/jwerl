# File: lib/Jwerl.ex
# This file was generated from src/jwerl.erl
# Using mix.mk (https://github.com/botsunit/mix.mk)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Jwerl do
  def unquote(:"sign")(arg1) do
    :erlang.apply(:"jwerl", :"sign", [arg1])
  end
  def unquote(:"sign")(arg1, arg2) do
    :erlang.apply(:"jwerl", :"sign", [arg1, arg2])
  end
  def unquote(:"verify")(arg1) do
    :erlang.apply(:"jwerl", :"verify", [arg1])
  end
  def unquote(:"verify")(arg1, arg2) do
    :erlang.apply(:"jwerl", :"verify", [arg1, arg2])
  end
end
