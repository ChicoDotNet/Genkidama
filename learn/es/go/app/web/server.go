package web

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"

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
}

// NewServer creates an HTTP handler for the configured targets.
func NewServer(checker BatchChecker, targets []monitor.Target, concurrency int) (*Server, error) {
	if checker == nil {
		return nil, fmt.Errorf("web: checker is required")
	}
	if concurrency < 1 {
		return nil, fmt.Errorf("web: concurrency must be at least 1")
	}
	return &Server{checker: checker, targets: append([]monitor.Target(nil), targets...), concurrency: concurrency}, nil
}

// Handler returns the complete HTTP handler for the application.
func (s *Server) Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /health", func(w http.ResponseWriter, _ *http.Request) { w.WriteHeader(http.StatusNoContent) })
	mux.HandleFunc("GET /api/checks", s.handleChecks)
	mux.HandleFunc("GET /", s.handleDashboard)
	return mux
}

func (s *Server) handleChecks(w http.ResponseWriter, r *http.Request) {
	results, err := s.checker.CheckAll(r.Context(), s.targets, s.concurrency)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	if err := json.NewEncoder(w).Encode(results); err != nil {
		return
	}
}

func (s *Server) handleDashboard(w http.ResponseWriter, _ *http.Request) {
	w.Header().Set("Content-Type", "text/html; charset=utf-8")
	_, _ = w.Write([]byte(`<!doctype html><html lang="es"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>UptimeLab</title></head><body><main><h1>UptimeLab</h1><p>Monitor concurrente local escrito en Go.</p><button id="refresh">Comprobar ahora</button><pre id="results">Sin ejecutar</pre></main><script>async function load(){const r=await fetch('/api/checks');document.querySelector('#results').textContent=JSON.stringify(await r.json(),null,2)}document.querySelector('#refresh').addEventListener('click',load);load();</script></body></html>`))
}
