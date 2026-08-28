services=%{clock:fn->"12:00" end}; unless services.clock.()=="12:00",do: raise "Locator"
