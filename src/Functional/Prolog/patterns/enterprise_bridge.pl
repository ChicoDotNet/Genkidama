sender(Text,Out):-atomic_list_concat(['sms:',Text],Out). notify(Text,Out):-sender(Text,Out). main:-notify(ok,'sms:ok'). :- initialization(main,main).
