null_log(_). service(Logger,ok):-call(Logger,run). main:-service(null_log,ok). :- initialization(main,main).
