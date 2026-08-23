defmodule DecoratorExample do
  def base_component, do: fn -> "alert" end
  def audit_decorator(component), do: fn -> "audit(#{component.()})" end
  def encrypt_decorator(component), do: fn -> "enc(#{component.()})" end

  def run do
    base = base_component()
    audited = audit_decorator(base)
    encrypted = encrypt_decorator(base)
    stacked = audit_decorator(encrypt_decorator(base))

    IO.puts("base=#{base.()}")
    IO.puts("audit=#{audited.()}")
    IO.puts("encrypted=#{encrypted.()}")
    IO.puts("stacked=#{stacked.()}")
  end
end

DecoratorExample.run()
