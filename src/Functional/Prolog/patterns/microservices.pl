inventory(Sku,inventory(Sku,available)). order(Sku):-inventory(Sku,inventory(Sku,available)). main:-order('A-1'). :- initialization(main,main).
