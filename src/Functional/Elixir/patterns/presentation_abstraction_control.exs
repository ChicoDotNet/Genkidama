control=fn a,d->Map.update!(a,:value,&(&1+d)) end; presentation=fn a->to_string(a.value) end; unless %{value:1}|>control.(2)|>presentation.()=="3",do: raise "PAC"
