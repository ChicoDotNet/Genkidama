mediator=fn s,m->{s,m} end; unless mediator.(:checkout,:paid)=={:checkout,:paid},do: raise "Mediator"
