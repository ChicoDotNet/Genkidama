package web

import (
	"sync"
	"time"
)

// MetricsSnapshot is an aggregate, privacy-preserving view of HTTP activity.
type MetricsSnapshot struct {
	Requests            int64 `json:"requests"`
	Failures            int64 `json:"failures"`
	TotalDurationMillis int64 `json:"total_duration_ms"`
}

// RequestMetrics records aggregate request counts and durations without storing request data.
type RequestMetrics struct {
	mu            sync.Mutex
	requests      int64
	failures      int64
	totalDuration time.Duration
}

// NewRequestMetrics creates an empty aggregate metrics collector.
func NewRequestMetrics() *RequestMetrics {
	return &RequestMetrics{}
}

// Observe records one completed request. HTTP 5xx responses count as failures.
func (m *RequestMetrics) Observe(status int, duration time.Duration) {
	if m == nil {
		return
	}
	if duration < 0 {
		duration = 0
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	m.requests++
	if status >= 500 {
		m.failures++
	}
	m.totalDuration += duration
}

// Snapshot returns a copy of aggregate metrics safe for concurrent readers.
func (m *RequestMetrics) Snapshot() MetricsSnapshot {
	if m == nil {
		return MetricsSnapshot{}
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return MetricsSnapshot{
		Requests:            m.requests,
		Failures:            m.failures,
		TotalDurationMillis: m.totalDuration.Milliseconds(),
	}
}
