class UnknownColleague < Exception
end

alias MediatorReceiver = Proc(String, String, Nil)

class CheckoutMediator
  def initialize
    @receivers = {} of String => MediatorReceiver
  end

  def register(name : String, receiver : MediatorReceiver) : Nil
    @receivers[name] = receiver
  end

  def send(sender : String, recipient : String, message : String) : Nil
    receiver = @receivers[recipient]?
    raise UnknownColleague.new("unknown colleague: #{recipient}") unless receiver
    receiver.call(sender, message)
  end
end

def verify_mediator : Nil
  payment_inbox = [] of String
  inventory_inbox = [] of String
  mediator = CheckoutMediator.new

  mediator.register("payment", ->(sender : String, message : String) do
    payment_inbox << "#{sender}:#{message}"
    nil
  end)
  mediator.register("inventory", ->(sender : String, message : String) do
    inventory_inbox << "#{sender}:#{message}"
    nil
  end)

  mediator.send("payment", "inventory", "paid")
  mediator.send("inventory", "payment", "reserved")

  raise "inventory routing failed" unless inventory_inbox == ["payment:paid"]
  raise "payment routing failed" unless payment_inbox == ["inventory:reserved"]

  begin
    mediator.send("payment", "shipping", "dispatch")
    raise "unknown colleague was accepted"
  rescue ex : UnknownColleague
    raise "unexpected failure message" unless ex.message == "unknown colleague: shipping"
  end

  puts "Crystal Mediator: passed"
end

verify_mediator
