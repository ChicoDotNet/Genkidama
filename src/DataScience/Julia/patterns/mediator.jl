# Mediator — colleagues communicate through a routing authority rather than directly.
mutable struct CheckoutMediator
    colleagues::Dict{String, Function}
end

CheckoutMediator() = CheckoutMediator(Dict{String, Function}())

function register!(mediator::CheckoutMediator, name::String, receiver::Function)
    mediator.colleagues[name] = receiver
    mediator
end

function send(mediator::CheckoutMediator, sender::String, recipient::String, message::String)
    receiver = get(mediator.colleagues, recipient, nothing)
    receiver === nothing && return (false, "unknown colleague: $recipient")
    (true, receiver(sender, message))
end

payment(sender::String, message::String) = "payment<-$sender:$message"
inventory(sender::String, message::String) = "inventory<-$sender:$message"

function verify_mediator()
    mediator = CheckoutMediator()
    register!(mediator, "payment", payment)
    register!(mediator, "inventory", inventory)

    paid = send(mediator, "payment", "inventory", "paid")
    reserved = send(mediator, "inventory", "payment", "reserved")
    missing = send(mediator, "payment", "shipping", "dispatch")

    @assert paid == (true, "inventory<-payment:paid")
    @assert reserved == (true, "payment<-inventory:reserved")
    @assert missing == (false, "unknown colleague: shipping")
    true
end

verify_mediator()
println("Julia Mediator: passed")
