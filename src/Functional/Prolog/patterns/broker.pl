handler(price,_Sku,9). request(Topic,Payload,R):-handler(Topic,Payload,R). main:-request(price,'A',9). :- initialization(main,main).
