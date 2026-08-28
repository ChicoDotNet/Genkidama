state = %{text: "draft"}
snapshot = state
_state = %{state | text: "edited"}
restored = snapshot

unless restored.text == "draft" do
  raise "Memento"
end
