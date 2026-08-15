package scheduler

import (
	"context"
	"errors"
	"time"
)

// Runner is a cancellable periodic scheduler around one operation.
type Runner struct {
	interval time.Duration
	run      func(context.Context) error
}

// New creates a Runner. The operation runs immediately and then once per interval.
func New(interval time.Duration, run func(context.Context) error) (*Runner, error) {
	if interval <= 0 {
		return nil, errors.New("scheduler: interval must be positive")
	}
	if run == nil {
		return nil, errors.New("scheduler: operation is required")
	}
	return &Runner{interval: interval, run: run}, nil
}

// Run executes until ctx is cancelled or an operation returns an error.
func (r *Runner) Run(ctx context.Context) error {
	if err := r.run(ctx); err != nil {
		return err
	}
	ticker := time.NewTicker(r.interval)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-ticker.C:
			if err := r.run(ctx); err != nil {
				return err
			}
		}
	}
}
