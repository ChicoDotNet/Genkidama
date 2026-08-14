package web

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"
	"time"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/history"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/insights"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
)

// BatchChecker runs a group of uptime checks.
type BatchChecker interface {
	CheckAll(context.Context, []monitor.Target, int) ([]monitor.Result, error)
}

// Server exposes the uptime monitor through HTTP JSON and a small dashboard.
type Server struct {
	checker     BatchChecker
	targets     []monitor.Target
	concurrency int
	history     *history.Log
	metrics     *RequestMetrics
	now         func() time.Time
}

// NewServer creates an HTTP handler without persistent history or diagnostics.
func NewServer(checker BatchChecker, targets []monitor.Target, concurrency int) (*Server, error) {
	return NewServerWithDiagnostics(checker, targets, concurrency, nil, nil, time.Now)
}

// NewServerWithHistory creates an HTTP handler and optionally records completed batches.
func NewServerWithHistory(checker BatchChecker, targets []monitor.Target, concurrency int, log *history.Log) (*Server, error) {
	return NewServerWithDiagnostics(checker, targets, concurrency, log, nil, time.Now)
}

// NewServerWithDiagnostics creates an HTTP handler with optional history and aggregate request diagnostics.
// The clock is injectable so duration-related tests remain deterministic.
func NewServerWithDiagnostics(checker BatchChecker, targets []monitor.Target, concurrency int, log *history.Log, metrics *RequestMetrics, now func() time.Time) (*Server, error) {
	if checker == nil {
		return nil, fmt.Errorf("web: checker is required")
	}
	if concurrency < 1 {
		return nil, fmt.Errorf("web: concurrency must be at least 1")
	}
	if now == nil {
		return nil, fmt.Errorf("web: clock is required")
	}
	return &Server{
		checker:     checker,
		targets:     append([]monitor.Target(nil), targets...),
		concurrency: concurrency,
		history:     log,
		metrics:     metrics,
		now:         now,
	}, nil
}

// RunChecks executes one batch and persists it before returning success.
func (s *Server) RunChecks(ctx context.Context) ([]monitor.Result, error) {
	results, err := s.checker.CheckAll(ctx, s.targets, s.concurrency)
	if err != nil {
		return nil, err
	}
	if s.history != nil {
		if err := s.history.Append(results); err != nil {
			return nil, fmt.Errorf("web: persist history: %w", err)
		}
	}
	return results, nil
}

// Handler returns the complete HTTP handler for the application.
func (s *Server) Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /health", func(w http.ResponseWriter, _ *http.Request) { w.WriteHeader(http.StatusNoContent) })
	mux.HandleFunc("GET /api/checks", s.handleChecks)
	mux.HandleFunc("GET /api/history", s.handleHistory)
	mux.HandleFunc("GET /api/summary", s.handleSummary)
	mux.HandleFunc("GET /api/trends", s.handleTrends)
	mux.HandleFunc("GET /api/diagnostics", s.handleDiagnostics)
	mux.HandleFunc("GET /", s.handleDashboard)

	var handler http.Handler = mux
	if s.metrics != nil {
		handler = s.observe(handler)
	}
	return securityHeaders(handler)
}

func (s *Server) handleChecks(w http.ResponseWriter, r *http.Request) {
	results, err := s.RunChecks(r.Context())
	if err != nil {
		http.Error(w, err.Error(), http.StatusServiceUnavailable)
		return
	}
	writeJSON(w, results)
}

func (s *Server) handleHistory(w http.ResponseWriter, _ *http.Request) {
	writeJSON(w, s.historyEntries())
}

func (s *Server) handleSummary(w http.ResponseWriter, _ *http.Request) {
	writeJSON(w, insights.Summarize(s.historyEntries()))
}

func (s *Server) handleTrends(w http.ResponseWriter, r *http.Request) {
	window := 5
	if raw := r.URL.Query().Get("window"); raw != "" {
		parsed, err := strconv.Atoi(raw)
		if err != nil || parsed < 1 || parsed > 100 {
			http.Error(w, "window must be an integer between 1 and 100", http.StatusBadRequest)
			return
		}
		window = parsed
	}
	trends, err := insights.Trends(s.historyEntries(), window)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	writeJSON(w, trends)
}

func (s *Server) handleDiagnostics(w http.ResponseWriter, _ *http.Request) {
	if s.metrics == nil {
		http.NotFound(w, nil)
		return
	}
	writeJSON(w, s.metrics.Snapshot())
}

func (s *Server) historyEntries() []monitor.Result {
	if s.history == nil {
		return []monitor.Result{}
	}
	return s.history.Entries()
}

func writeJSON(w http.ResponseWriter, value any) {
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	_ = json.NewEncoder(w).Encode(value)
}

func (s *Server) handleDashboard(w http.ResponseWriter, r *http.Request) {
	if r.URL.Path != "/" {
		http.NotFound(w, r)
		return
	}
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_, _ = w.Write([]byte(`<!doctype html><html lang="es"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>UptimeLab</title></head><body><main><h1>UptimeLab</h1><p>Monitor concurrente local escrito en Go.</p><button id="refresh">Comprobar ahora</button><h2>Último check</h2><pre id="results">Sin ejecutar</pre><h2>Resumen histórico</h2><pre id="summary">Sin historial</pre></main><script>async function load(){const r=await fetch('/api/checks');document.querySelector('#results').textContent=JSON.stringify(await r.json(),null,2);const s=await fetch('/api/summary');document.querySelector('#summary').textContent=JSON.stringify(await s.json(),null,2)}document.querySelector('#refresh').addEventListener('click',load);load();</script></body></html>`))
}

type statusWriter struct {
	http.ResponseWriter
	status int
}

func (w *statusWriter) WriteHeader(status int) {
	w.status = status
	w.ResponseWriter.WriteHeader(status)
}

func (w *statusWriter) Write(payload []byte) (int, error) {
	if w.status == 0 {
		w.status = http.StatusOK
	}
	return w.ResponseWriter.Write(payload)
}

func (s *Server) observe(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		started := s.now()
		tracked := &statusWriter{ResponseWriter: w, status: http.StatusOK}
		next.ServeHTTP(tracked, r)
		s.metrics.Observe(tracked.status, s.now().Sub(started))
	})
}

func securityHeaders(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("X-Content-Type-Options", "nosniff")
		w.Header().Set("Referrer-Policy", "no-referrer")
		w.Header().Set("Content-Security-Policy", "default-src 'self'; script-src 'self' 'unsafe-inline'; object-src 'none'; base-uri 'none'; frame-ancestors 'none'")
		next.ServeHTTP(w, r)
	})
}
