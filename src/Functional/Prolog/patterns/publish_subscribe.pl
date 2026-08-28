subscribe(news,reader). publish(news,V,reader-received(V)). main:-publish(news,v1,reader-received(v1)). :- initialization(main,main).
