package main

import "fmt"

type DocumentStore interface {
	Get(id int) string
}

type RemoteDocumentStore struct {
	fetchCount int
}

func (s *RemoteDocumentStore) Get(id int) string {
	s.fetchCount++
	return fmt.Sprintf("doc(%d)", id)
}

type DocumentStoreProxy struct {
	backend *RemoteDocumentStore
	cache   map[int]string
}

func NewDocumentStoreProxy() *DocumentStoreProxy {
	return &DocumentStoreProxy{cache: make(map[int]string)}
}

func (p *DocumentStoreProxy) Get(id int) string {
	if value, ok := p.cache[id]; ok {
		return value
	}
	if p.backend == nil {
		p.backend = &RemoteDocumentStore{}
	}
	value := p.backend.Get(id)
	p.cache[id] = value
	return value
}

func (p *DocumentStoreProxy) BackendCount() int {
	if p.backend == nil {
		return 0
	}
	return 1
}

func (p *DocumentStoreProxy) FetchCount() int {
	if p.backend == nil {
		return 0
	}
	return p.backend.fetchCount
}

func main() {
	store := NewDocumentStoreProxy()
	first := store.Get(42)
	second := store.Get(42)
	fmt.Printf("backend=%d;fetches=%d;first=%s;second=%s\n", store.BackendCount(), store.FetchCount(), first, second)
}
