mediator = fn sender, message -> {sender, message} end

unless mediator.(:checkout, :paid) == {:checkout, :paid} do
  raise "Mediator"
end
