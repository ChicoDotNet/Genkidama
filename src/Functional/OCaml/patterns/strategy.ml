let choose xs strategy=strategy xs;;let ()=assert(choose[3;1;2](List.fold_left min max_int)=1);assert(choose[3;1;2](List.fold_left max min_int)=3)
