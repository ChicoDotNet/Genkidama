colleague(payment).
colleague(inventory).

receive(inventory, payment, paid, received(inventory, payment, paid)).
receive(payment, inventory, reserved, received(payment, inventory, reserved)).

mediate(Sender, Recipient, Message, Delivery) :-
    colleague(Recipient),
    receive(Recipient, Sender, Message, Delivery).

main :-
    mediate(payment, inventory, paid, received(inventory, payment, paid)),
    mediate(inventory, payment, reserved, received(payment, inventory, reserved)),
    \+ mediate(payment, shipping, paid, _).

:- initialization(main, main).
