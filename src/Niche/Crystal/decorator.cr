alias Render = Proc(String)

plain = -> { "alert" }
audit = ->(inner : Render) { -> { "audit(#{inner.call})" } }
encrypt = ->(inner : Render) { -> { "enc(#{inner.call})" } }

audited = audit.call(plain)
encrypted = encrypt.call(plain)
stacked = audit.call(encrypt.call(plain))

puts "base=#{plain.call}"
puts "audit=#{audited.call}"
puts "encrypted=#{encrypted.call}"
puts "stacked=#{stacked.call}"
