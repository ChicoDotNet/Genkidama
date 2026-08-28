module RepositoryExample
let run ()=[1,"Ada";2,"Grace"]|>List.find(fun(id,_)->id=2)|>snd="Grace"
