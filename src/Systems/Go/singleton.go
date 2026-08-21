package main

import (
	"fmt"
	"sync"
)

type processRegistry struct {
	count int
}

var (
	registryOnce sync.Once
	registryInst *processRegistry
)

func registry() *processRegistry {
	registryOnce.Do(func() { registryInst = &processRegistry{} })
	return registryInst
}

func main() {
	first := registry()
	second := registry()
	first.count++
	fmt.Printf("same=%t\n", first == second)
	fmt.Printf("count=%d\n", second.count)
}
