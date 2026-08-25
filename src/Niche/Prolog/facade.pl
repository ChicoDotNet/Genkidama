auth_service(User, Result) :-
    format(string(Result), 'auth(~w)', [User]).

inventory_service(Sku, Result) :-
    format(string(Result), 'reserve(~w)', [Sku]).

billing_service(Amount, Result) :-
    format(string(Result), 'charge(~w)', [Amount]).

checkout_facade(User, Sku, Amount, Result) :-
    auth_service(User, Auth),
    inventory_service(Sku, Inventory),
    billing_service(Amount, Billing),
    format(string(Result), '~w>~w>~w', [Auth, Inventory, Billing]).

main :-
    checkout_facade(alice, 'SKU-42', 499, Result),
    format('checkout=~w~n', [Result]).
