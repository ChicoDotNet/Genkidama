package web

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"strconv"

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
}

// NewServer creates an HTTP handler without persistent history.
func NewServer(checker BatchChecker, targets []monitor.Target, concurrency int) (*Server, error) {
	return NewServerWithHistory(checker, targets, concurrency, nil)
}

// NewServerWithHistory creates an HTTP handler and optionally records completed batches.
func NewServerWithHistory(checker BatchChecker, targets []monitor.Target, concurrency int, log *history.Log) (*Server, error) {
	if checker == nil {
		return nil, fmt.Errorf("web: checker is required")
	}
	if concurrency < 1 {
		return nil, fmt.Errorf("web: concurrency must be at least 1")
	}
	return &Server{
		checker:     checker,
		targets:     append([]monitor.Target(nil), targets...),
		concurrency: concurrency,
		history:     log,
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
	mux.HandleFunc("GET /", s.handleDashboard)
	return mux
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

func (s *Server) handleDashboard(w http.ResponseWriter, _ *http.Request) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_, _ = w.Write([]byte(`<!doctype html><html lang="es"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>UptimeLab</title></head><body><main><h1>UptimeLab</h1><p>Monitor concurrente local escrito en Go.</p><button id="refresh">Comprobar ahora</button><h2>Último check</h2><pre id="results">Sin ejecutar</pre><h2>Resumen histórico</h2><pre id="summary">Sin historial</pre></main><script>async function load(){const r=await fetch('/api/checks');document.querySelector('#results').textContent=JSON.stringify(await r.json(),null,2);const s=await fetch('/api/summary');document.querySelector('#summary').textContent=JSON.stringify(await s.json(),null,2)}document.querySelector('#refresh').addEventListener('click',load);load();</script></body></html>`))
}
