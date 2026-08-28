guarded=fn state->Map.update!(state,:value,&(&1+1)) end; unless guarded.(%{value:0}).value==1,do: raise "Monitor"
