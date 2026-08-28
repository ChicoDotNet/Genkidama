let () = let b=ref 100 in let cmds=[(fun()->b:=!b+50);(fun()->b:=!b-20)] in List.iter(fun f->f())cmds; assert(!b=130)
