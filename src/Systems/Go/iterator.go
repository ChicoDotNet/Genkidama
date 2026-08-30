package main

import "fmt"

type Iterator[T any] struct {
	values []T
	index  int
}

func (it *Iterator[T]) Next() (T, bool) {
	if it.index >= len(it.values) {
		var zero T
		return zero, false
	}
	value := it.values[it.index]
	it.index++
	return value, true
}

func main() {
	it := Iterator[int]{values: []int{10, 20, 30}}
	visited := make([]int, 0, 3)
	for {
		value, ok := it.Next()
		if !ok {
			break
		}
		visited = append(visited, value)
	}
	if len(visited) != 3 || visited[0] != 10 || visited[1] != 20 || visited[2] != 30 {
		panic("iterator contract failed")
	}
	fmt.Println("iterator=10,20,30")
}
