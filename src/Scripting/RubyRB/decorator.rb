class PlainMessage
  def render
    'alert'
  end
end

class ComponentDecorator
  def initialize(inner)
    @inner = inner
  end
end

class AuditDecorator < ComponentDecorator
  def render
    "audit(#{@inner.render})"
  end
end

class EncryptDecorator < ComponentDecorator
  def render
    "enc(#{@inner.render})"
  end
end

component = PlainMessage.new
puts "base=#{component.render}"
puts "audit=#{AuditDecorator.new(component).render}"
puts "encrypted=#{EncryptDecorator.new(component).render}"
puts "stacked=#{AuditDecorator.new(EncryptDecorator.new(component)).render}"
