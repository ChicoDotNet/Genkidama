visit=fn %{value:v}->v*2 end; unless visit.(%{value:5})==10,do: raise "Visitor"
