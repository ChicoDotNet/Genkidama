package monitor

import (
	"context"
	"net/http"
	"net/http/httptest"
	"sync/atomic"
	"testing"
	"time"
)

func TestCheckHealthyTarget(t *testing.T) {
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { w.WriteHeader(http.StatusNoContent) }))
	defer server.Close()

	checker := NewChecker(server.Client())
	result := checker.Check(context.Background(), Target{Name: "local", URL: server.URL})
	if !result.Healthy() || result.StatusCode != http.StatusNoContent {
		t.Fatalf("unexpected result: %+v", result)
	}
}

func TestCheckRejectsInvalidURL(t *testing.T) {
	result := NewChecker(nil).Check(context.Background(), Target{Name: "bad", URL: "ftp://example.test"})
	if result.Error == "" {
		t.Fatal("expected validation error")
	}
}

func TestCheckAllPreservesOrderAndLimitsConcurrency(t *testing.T) {
	var active atomic.Int32
	var maximum atomic.Int32
	server := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) {
		current := active.Add(1)
		defer active.Add(-1)
		for {
			seen := maximum.Load()
			if current <= seen || maximum.CompareAndSwap(seen, current) {
				break
			}
		}
		time.Sleep(15 * time.Millisecond)
		w.WriteHeader(http.StatusOK)
	}))
	defer server.Close()

	targets := []Target{{Name: "one", URL: server.URL}, {Name: "two", URL: server.URL}, {Name: "three", URL: server.URL}}
	results, err := NewChecker(server.Client()).CheckAll(context.Background(), targets, 2)
	if err != nil {
		t.Fatal(err)
	}
	for i := range targets {
		if results[i].Target.Name != targets[i].Name {
			t.Fatalf("order changed: %+v", results)
		}
	}
	if maximum.Load() > 2 {
		t.Fatalf("concurrency exceeded limit: %d", maximum.Load())
	}
}

func TestCheckAllRejectsInvalidConcurrency(t *testing.T) {
	if _, err := NewChecker(nil).CheckAll(context.Background(), nil, 0); err == nil {
		t.Fatal("expected concurrency validation error")
	}
}
