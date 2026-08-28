presenter=fn model,view->Map.put(view,:text,String.upcase(model.name)) end; unless presenter.(%{name:"Ada"},%{}).text=="ADA",do: raise "MVP"
