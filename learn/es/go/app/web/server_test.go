package web

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
)

type fakeChecker struct{ results []monitor.Result }

func (f fakeChecker) CheckAll(context.Context, []monitor.Target, int) ([]monitor.Result, error) {
	return f.results, nil
}

func TestChecksEndpointReturnsJSON(t *testing.T) {
	server, err := NewServer(fakeChecker{results: []monitor.Result{{Target: monitor.Target{Name: "api", URL: "https://example.test"}, StatusCode: 200}}}, []monitor.Target{{Name: "api", URL: "https://example.test"}}, 2)
	if err != nil {
		t.Fatal(err)
	}
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/api/checks", nil))
	if recorder.Code != http.StatusOK {
		t.Fatalf("status = %d", recorder.Code)
	}
	var results []monitor.Result
	if err := json.NewDecoder(recorder.Body).Decode(&results); err != nil {
		t.Fatal(err)
	}
	if len(results) != 1 || results[0].Target.Name != "api" {
		t.Fatalf("unexpected payload: %+v", results)
	}
}

func TestHealthEndpoint(t *testing.T) {
	server, err := NewServer(fakeChecker{}, nil, 1)
	if err != nil {
		t.Fatal(err)
	}
	recorder := httptest.NewRecorder()
	server.Handler().ServeHTTP(recorder, httptest.NewRequest(http.MethodGet, "/health", nil))
	if recorder.Code != http.StatusNoContent {
		t.Fatalf("status = %d", recorder.Code)
	}
}
