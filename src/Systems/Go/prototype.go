package main

import (
	"fmt"
	"strings"
)

type Prototype[T any] interface {
	Clone() T
}

type ServiceProfile struct {
	Name     string
	Features []string
}

func (profile ServiceProfile) Clone() ServiceProfile {
	features := append([]string(nil), profile.Features...)
	return ServiceProfile{Name: profile.Name, Features: features}
}

func (profile ServiceProfile) Describe() string {
	return fmt.Sprintf("%s: %s", profile.Name, strings.Join(profile.Features, ","))
}

func main() {
	var prototype Prototype[ServiceProfile] = ServiceProfile{Name: "orders", Features: []string{"metrics"}}
	original := prototype.(ServiceProfile)
	canary := prototype.Clone()

	canary.Name = "orders-canary"
	canary.Features = append(canary.Features, "tracing")

	fmt.Printf("original=%s\n", original.Describe())
	fmt.Printf("clone=%s\n", canary.Describe())
}
