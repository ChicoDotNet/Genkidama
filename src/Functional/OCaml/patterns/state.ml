let ()=let state=ref`Closed in let toggle()=state:=match!state with`Closed->`Open|`Open->`Closed in toggle();assert(!state=`Open)
