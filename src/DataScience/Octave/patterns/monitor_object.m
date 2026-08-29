function monitor_object(); state.value=0; guarded=@(s)struct('value',s.value+1); state=guarded(state); assert(state.value==1); end
