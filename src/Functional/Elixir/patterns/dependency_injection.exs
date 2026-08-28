greet=fn clock->"hello@#{clock.()}" end; unless greet.(fn->"noon" end)=="hello@noon",do: raise "DI"
