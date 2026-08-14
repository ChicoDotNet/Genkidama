package web

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/history"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/insights"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
)

type fakeChecker struct {
	results []monitor.Result
}

func (f fakeChecker) CheckAll(context.Context, []monitor.Target, int) ([]monitor.Result, error) {
	return f.results, nil
}

type memoryStore struct {
	entries []monitor.Result
	saveErr error
}

func (s *memoryStore) Load() ([]monitor.Result, error) {
	return append([]monitor.Result(nil), s.entries...), nil
}

func (s *memoryStore) Save(value []monitor.Result) error {
	if s.saveErr != nil {
		return s.saveErr
	}
	s.entries = append([]monitor.Result(nil), value...)
	return nil
}

func TestChecksEndpointReturnsJSONAndPersistsHistory(t *testing.T) {
	store := &memoryStore{}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(
		fakeChecker{results: []monitor.Result{{Target: monitor.Target{Name: "api", URL: "https://example.test"}, StatusCode: 200}}},
		[]monitor.Target{{Name: "api", URL: "https://example.test"}},
		2,
		log,
	)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/checks", nil))
	if recorder.Code != http.StatusOK {
		t.Fatalf("status = %d body = %s", recorder.Code, recorder.Body.String())
	}
	if len(log.Entries()) != 1 {
		t.Fatal("expected persisted result")
	}
}

func TestHistoryEndpointReturnsRecordedResults(t *testing.T) {
	store := &memoryStore{entries: []monitor.Result{{Target: monitor.Target{Name: "old"}}}}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(fakeChecker{}, nil, 1, log)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/history", nil))
	var got []monitor.Result
	if err := json.NewDecoder(recorder.Body).Decode(&got); err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].Target.Name != "old" {
		t.Fatalf("unexpected history: %+v", got)
	}
}

func TestSummaryEndpointDerivesCurrentReliability(t *testing.T) {
	target := monitor.Target{Name: "api", URL: "https://api.example.test"}
	store := &memoryStore{entries: []monitor.Result{
		{Target: target, StatusCode: 200, Latency: 10 * time.Millisecond},
		{Target: target, StatusCode: 500, Latency: 30 * time.Millisecond},
	}}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(fakeChecker{}, nil, 1, log)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/summary", nil))
	var got []insights.Summary
	if err := json.NewDecoder(recorder.Body).Decode(&got); err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].AvailabilityPercent != 50 || got[0].ConsecutiveFailures != 1 {
		t.Fatalf("summary = %+v", got)
	}
}

func TestTrendsEndpointUsesValidatedWindow(t *testing.T) {
	target := monitor.Target{Name: "api", URL: "https://api.example.test"}
	store := &memoryStore{entries: []monitor.Result{
		{Target: target, StatusCode: 200},
		{Target: target, StatusCode: 200},
		{Target: target, StatusCode: 500},
		{Target: target, StatusCode: 500},
	}}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(fakeChecker{}, nil, 1, log)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/trends?window=2", nil))
	var got []insights.Trend
	if err := json.NewDecoder(recorder.Body).Decode(&got); err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].DeltaPercent != -100 {
		t.Fatalf("trends = %+v", got)
	}
}

func TestTrendsEndpointRejectsInvalidWindow(t *testing.T) {
	server, _ := NewServer(fakeChecker{}, nil, 1)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/trends?window=0", nil))
	if recorder.Code != http.StatusBadRequest {
		t.Fatalf("status = %d", recorder.Code)
	}
}

func TestChecksEndpointDoesNotPublishHistoryWhenPersistenceFails(t *testing.T) {
	store := &memoryStore{
		entries: []monitor.Result{{Target: monitor.Target{Name: "old"}}},
		saveErr: errors.New("disk unavailable"),
	}
	log, _ := history.NewLog(store, 10)
	server, _ := NewServerWithHistory(
		fakeChecker{results: []monitor.Result{{Target: monitor.Target{Name: "new"}}}},
		nil,
		1,
		log,
	)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/checks", nil))
	if recorder.Code != http.StatusServiceUnavailable {
		t.Fatalf("status = %d", recorder.Code)
	}
	got := log.Entries()
	if len(got) != 1 || got[0].Target.Name != "old" {
		t.Fatalf("visible history changed: %+v", got)
	}
}

func TestHealthEndpoint(t *testing.T) {
	server, _ := NewServer(fakeChecker{}, nil, 1)
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/health", nil))
	if recorder.Code != http.StatusNoContent {
		t.Fatalf("status = %d", recorder.Code)
	}
}
