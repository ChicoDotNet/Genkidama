package scheduler

import (
	"context"
	"errors"
	"sync/atomic"
	"testing"
	"time"
)

func TestRunnerExecutesImmediatelyAndStopsOnCancellation(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	var calls atomic.Int32
	runner, err := New(time.Hour, func(context.Context) error {
		calls.Add(1)
		cancel()
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	if err := runner.Run(ctx); !errors.Is(err, context.Canceled) {
		t.Fatalf("error = %v", err)
	}
	if calls.Load() != 1 {
		t.Fatalf("calls = %d", calls.Load())
	}
}

func TestRunnerRejectsInvalidConfiguration(t *testing.T) {
	if _, err := New(0, func(context.Context) error { return nil }); err == nil {
		t.Fatal("expected interval error")
	}
	if _, err := New(time.Second, nil); err == nil {
		t.Fatal("expected operation error")
	}
}
