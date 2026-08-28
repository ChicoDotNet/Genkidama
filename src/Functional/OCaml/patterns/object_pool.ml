let ()=let item=ref 1 in let pool=ref[item]in let borrowed=List.hd!pool in pool:=[borrowed];assert(List.hd!pool==item)
