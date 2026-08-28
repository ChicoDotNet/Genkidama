controller=fn m->Map.update!(m,:count,&(&1+1)) end; view=fn m->"count=#{m.count}" end; unless %{count:0}|>controller.()|>view.()=="count=1",do: raise "MVC"
