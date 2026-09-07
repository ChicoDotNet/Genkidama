package main

import "fmt"

type gateState interface {
	name() string
	coin(*turnstile) string
	push(*turnstile) string
}

type lockedState struct{}
type unlockedState struct{}

type turnstile struct {
	state gateState
}

func newTurnstile() *turnstile {
	return &turnstile{state: lockedState{}}
}

func (lockedState) name() string { return "locked" }
func (lockedState) coin(t *turnstile) string {
	t.state = unlockedState{}
	return "unlocked"
}
func (lockedState) push(*turnstile) string { return "blocked" }

func (unlockedState) name() string           { return "unlocked" }
func (unlockedState) coin(*turnstile) string { return "coin-returned" }
func (unlockedState) push(t *turnstile) string {
	t.state = lockedState{}
	return "passed"
}

func require(ok bool) {
	if !ok {
		panic("Go State contract failed")
	}
}

func main() {
	gate := newTurnstile()
	require(gate.state.name() == "locked")

	require(gate.state.push(gate) == "blocked")
	require(gate.state.name() == "locked")

	require(gate.state.coin(gate) == "unlocked")
	require(gate.state.name() == "unlocked")

	require(gate.state.coin(gate) == "coin-returned")
	require(gate.state.name() == "unlocked")

	require(gate.state.push(gate) == "passed")
	require(gate.state.name() == "locked")

	fmt.Println("go-state: passed")
}
