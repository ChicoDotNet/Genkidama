state=%{text:"draft"}; snapshot=state; state=%{state|text:"edited"}; state=snapshot; unless state.text=="draft",do: raise "Memento"
