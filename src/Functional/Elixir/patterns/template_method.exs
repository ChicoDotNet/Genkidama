render=fn body->"<#{body.()}>" end; unless render.(fn->"sales" end)=="<sales>",do: raise "Template"
