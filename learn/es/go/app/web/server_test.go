package web

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/history"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/insights"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
)

type fakeChecker struct {
	results []monitor.Result
	err     error
}

func (f fakeChecker) CheckAll(context.Context, []monitor.Target, int) ([]monitor.Result, error) {
	return f.results, f.err
}

type memoryStore struct {
	entries []monitor.Result
	saveErr error
}

func (s *memoryStore) Load() ([]monitor.Result, error) { return append([]monitor.Result(nil), s.entries...), nil }
func (s *memoryStore) Save(value []monitor.Result) error {
	if s.saveErr != nil { return s.saveErr }
	s.entries = append([]monitor.Result(nil), value...)
	return nil
}

func TestChecksEndpointReturnsJSONAndPersistsHistory(t *testing.T) {
	store := &memoryStore{}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(fakeChecker{results: []monitor.Result{{Target: monitor.Target{Name: "api", URL: "https://example.test"}, StatusCode: 200}}}, []monitor.Target{{Name: "api", URL: "https://example.test"}}, 2, log)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/checks", nil))
	if recorder.Code != http.StatusOK { t.Fatalf("status = %d body = %s", recorder.Code, recorder.Body.String()) }
	if len(log.Entries()) != 1 { t.Fatal("expected persisted result") }
}

func TestHistoryEndpointReturnsRecordedResults(t *testing.T) {
	store := &memoryStore{entries: []monitor.Result{{Target: monitor.Target{Name: "old"}}}}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(fakeChecker{}, nil, 1, log)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/history", nil))
	var got []monitor.Result
	if err := json.NewDecoder(recorder.Body).Decode(&got); err != nil { t.Fatal(err) }
	if len(got) != 1 || got[0].Target.Name != "old" { t.Fatalf("unexpected history: %+v", got) }
}

func TestSummaryEndpointDerivesCurrentReliability(t *testing.T) {
	target := monitor.Target{Name: "api", URL: "https://api.example.test"}
	store := &memoryStore{entries: []monitor.Result{{Target: target, StatusCode: 200, Latency: 10 * time.Millisecond}, {Target: target, StatusCode: 500, Latency: 30 * time.Millisecond}}}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(fakeChecker{}, nil, 1, log)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/summary", nil))
	var got []insights.Summary
	if err := json.NewDecoder(recorder.Body).Decode(&got); err != nil { t.Fatal(err) }
	if len(got) != 1 || got[0].AvailabilityPercent != 50 || got[0].ConsecutiveFailures != 1 { t.Fatalf("summary = %+v", got) }
}

func TestTrendsEndpointUsesValidatedWindow(t *testing.T) {
	target := monitor.Target{Name: "api", URL: "https://api.example.test"}
	store := &memoryStore{entries: []monitor.Result{{Target: target, StatusCode: 200}, {Target: target, StatusCode: 200}, {Target: target, StatusCode: 500}, {Target: target, StatusCode: 500}}}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(fakeChecker{}, nil, 1, log)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/trends?window=2", nil))
	var got []insights.Trend
	if err := json.NewDecoder(recorder.Body).Decode(&got); err != nil { t.Fatal(err) }
	if len(got) != 1 || got[0].DeltaPercent != -100 { t.Fatalf("trends = %+v", got) }
}

func TestTrendsEndpointRejectsInvalidWindow(t *testing.T) {
	server, _ := NewServer(fakeChecker{}, nil, 1)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/trends?window=0", nil))
	if recorder.Code != http.StatusBadRequest { t.Fatalf("status = %d", recorder.Code) }
}

func TestChecksEndpointDoesNotPublishHistoryWhenPersistenceFails(t *testing.T) {
	store := &memoryStore{entries: []monitor.Result{{Target: monitor.Target{Name: "old"}}}, saveErr: errors.New("disk unavailable")}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(fakeChecker{results: []monitor.Result{{Target: monitor.Target{Name: "new"}}}}, nil, 1, log)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/checks", nil))
	if recorder.Code != http.StatusServiceUnavailable { t.Fatalf("status = %d", recorder.Code) }
	got := log.Entries()
	if len(got) != 1 || got[0].Target.Name != "old" { t.Fatalf("visible history changed: %+v", got) }
}

func TestHealthEndpoint(t *testing.T) {
	server, _ := NewServer(fakeChecker{}, nil, 1)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/health", nil))
	if recorder.Code != http.StatusNoContent { t.Fatalf("status = %d", recorder.Code) }
}

func TestHandlerAddsDefensiveHeadersAndRejectsUnknownRoutes(t *testing.T) {
	server, _ := NewServer(fakeChecker{}, nil, 1)
	handler := server.Handler()
	health := httptest.NewRecorder()
	handler.ServeHTTP(health, httptest.NewRequest(http.MethodGet, "/health", nil))
	if health.Header().Get("X-Content-Type-Options") != "nosniff" { t.Fatal("missing nosniff") }
	if health.Header().Get("Referrer-Policy") != "no-referrer" { t.Fatal("missing referrer policy") }
	if health.Header().Get("Content-Security-Policy") == "" { t.Fatal("missing CSP") }
	unknown := httptest.NewRecorder()
	handler.ServeHTTP(unknown, httptest.NewRequest(http.MethodGet, "/not-a-route", nil))
	if unknown.Code != http.StatusNotFound { t.Fatalf("unknown route status = %d", unknown.Code) }
}

func TestDiagnosticsAreOptInAndAggregateOnly(t *testing.T) {
	withoutMetrics, _ := NewServer(fakeChecker{}, nil, 1)
	disabled := httptest.NewRecorder()
	withoutMetrics.Handler().ServeHTTP(disabled, httptest.NewRequest(http.MethodGet, "/api/diagnostics", nil))
	if disabled.Code != http.StatusNotFound { t.Fatalf("disabled diagnostics status = %d", disabled.Code) }

	metrics := NewRequestMetrics()
	base := time.Unix(1_700_000_000, 0)
	step := 0
	clock := func() time.Time { current := base.Add(time.Duration(step) * 25 * time.Millisecond); step++; return current }
	server, err := NewServerWithDiagnostics(fakeChecker{err: errors.New("network unavailable")}, []monitor.Target{{Name: "private-api", URL: "https://secret.example.test/path"}}, 1, nil, metrics, clock)
	if err != nil { t.Fatal(err) }
	failed := httptest.NewRecorder()
	server.Handler().ServeHTTP(failed, httptest.NewRequest(http.MethodGet, "/api/checks", nil))
	if failed.Code != http.StatusServiceUnavailable { t.Fatalf("failed check status = %d", failed.Code) }

	diagnostics := httptest.NewRecorder()
	server.Handler().ServeHTTP(diagnostics, httptest.NewRequest(http.MethodGet, "/api/diagnostics", nil))
	raw := diagnostics.Body.String()
	if strings.Contains(raw, "private-api") || strings.Contains(raw, "secret.example.test") { t.Fatalf("diagnostics leaked target data: %s", raw) }
	var got MetricsSnapshot
	if err := json.Unmarshal([]byte(raw), &got); err != nil { t.Fatal(err) }
	if got.Requests != 1 || got.Failures != 1 || got.TotalDurationMillis != 25 { t.Fatalf("diagnostics = %+v", got) }
}
