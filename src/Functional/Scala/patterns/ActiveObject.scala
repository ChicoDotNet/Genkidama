object ActiveObjectExample { def run:Boolean={var v=0;val q=List[()=>Unit](()=>v+=3,()=>v*=4);val before=v;q.foreach(_());before==0&&v==12} }
