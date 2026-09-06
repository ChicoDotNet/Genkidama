-module(mediator).
-export([main/0]).

new() ->
    #{payment => fun payment/1,
      inventory => fun inventory/1}.

send(Mediator, Recipient, Message) ->
    case maps:find(Recipient, Mediator) of
        {ok, Receiver} -> {ok, Receiver(Message)};
        error -> {error, unknown_colleague}
    end.

payment({from, inventory, reserved}) -> payment_ack;
payment(_) -> ignored.

inventory({from, payment, paid}) -> reserve_stock;
inventory(_) -> ignored.

main() ->
    Mediator = new(),
    {ok, reserve_stock} = send(Mediator, inventory, {from, payment, paid}),
    {ok, payment_ack} = send(Mediator, payment, {from, inventory, reserved}),
    {error, unknown_colleague} = send(Mediator, shipping, {from, payment, paid}),
    ok.
