package monitor

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"sync"
	"time"
)

// Target identifies one HTTP endpoint to monitor.
type Target struct {
	Name string `json:"name"`
	URL  string `json:"url"`
}

// Result records one completed uptime check.
type Result struct {
	Target     Target        `json:"target"`
	StatusCode int           `json:"statusCode"`
	Latency    time.Duration `json:"latency"`
	Error      string        `json:"error,omitempty"`
	CheckedAt  time.Time     `json:"checkedAt"`
}

// Healthy reports whether the check completed with a 2xx or 3xx HTTP status.
func (r Result) Healthy() bool {
	return r.Error == "" && r.StatusCode >= 200 && r.StatusCode < 400
}

// Doer is the minimal HTTP client contract required by Checker.
type Doer interface {
	Do(*http.Request) (*http.Response, error)
}

// Checker performs HTTP uptime checks using an injected client and clock.
type Checker struct {
	client Doer
	now    func() time.Time
}

// NewChecker creates a checker. A nil client uses an HTTP client with a five-second timeout.
func NewChecker(client Doer) *Checker {
	if client == nil {
		client = &http.Client{Timeout: 5 * time.Second}
	}
	return &Checker{client: client, now: time.Now}
}

// NewCheckerWithClock creates a checker with an injected clock for deterministic tests.
func NewCheckerWithClock(client Doer, now func() time.Time) (*Checker, error) {
	if client == nil {
		return nil, errors.New("monitor: client is required")
	}
	if now == nil {
		return nil, errors.New("monitor: clock is required")
	}
	return &Checker{client: client, now: now}, nil
}

// Check validates and probes one target. Operational failures are returned in Result.Error.
func (c *Checker) Check(ctx context.Context, target Target) Result {
	checkedAt := c.now()
	result := Result{Target: target, CheckedAt: checkedAt}
	if target.Name == "" {
		result.Error = "target name is required"
		return result
	}
	parsed, err := url.ParseRequestURI(target.URL)
	if err != nil || (parsed.Scheme != "http" && parsed.Scheme != "https") || parsed.Host == "" {
		result.Error = "target URL must be absolute http(s)"
		return result
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, target.URL, nil)
	if err != nil {
		result.Error = err.Error()
		return result
	}
	start := c.now()
	resp, err := c.client.Do(req)
	result.Latency = c.now().Sub(start)
	if err != nil {
		result.Error = err.Error()
		return result
	}
	defer resp.Body.Close()
	result.StatusCode = resp.StatusCode
	return result
}

// CheckAll checks targets concurrently while preserving input order.
func (c *Checker) CheckAll(ctx context.Context, targets []Target, concurrency int) ([]Result, error) {
	if concurrency < 1 {
		return nil, fmt.Errorf("monitor: concurrency must be at least 1")
	}
	results := make([]Result, len(targets))
	semaphore := make(chan struct{}, concurrency)
	var wg sync.WaitGroup
	for i, target := range targets {
		i, target := i, target
		wg.Add(1)
		go func() {
			defer wg.Done()
			select {
			case semaphore <- struct{}{}:
				defer func() { <-semaphore }()
			case <-ctx.Done():
				results[i] = Result{Target: target, Error: ctx.Err().Error(), CheckedAt: c.now()}
				return
			}
			results[i] = c.Check(ctx, target)
		}()
	}
	wg.Wait()
	return results, nil
}
