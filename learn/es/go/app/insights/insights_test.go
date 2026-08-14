package insights

import (
	"testing"
	"time"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
)

func TestSummarizeDerivesAvailabilityLatencyAndFailureStreak(t *testing.T) {
	t0 := time.Date(2026, 8, 14, 12, 0, 0, 0, time.UTC)
	api := monitor.Target{Name: "api", URL: "https://api.example.test"}
	web := monitor.Target{Name: "web", URL: "https://web.example.test"}
	results := []monitor.Result{
		{Target: web, StatusCode: 200, Latency: 20 * time.Millisecond, CheckedAt: t0},
		{Target: api, StatusCode: 200, Latency: 10 * time.Millisecond, CheckedAt: t0},
		{Target: api, StatusCode: 500, Latency: 30 * time.Millisecond, CheckedAt: t0.Add(time.Minute)},
		{Target: api, Error: "timeout", Latency: 50 * time.Millisecond, CheckedAt: t0.Add(2 * time.Minute)},
	}

	got := Summarize(results)
	if len(got) != 2 {
		t.Fatalf("len = %d", len(got))
	}
	if got[0].Target.Name != "api" {
		t.Fatalf("first target = %q", got[0].Target.Name)
	}
	if got[0].Samples != 3 || got[0].Healthy != 1 {
		t.Fatalf("summary = %+v", got[0])
	}
	if got[0].AvailabilityPercent != 100.0/3.0 {
		t.Fatalf("availability = %v", got[0].AvailabilityPercent)
	}
	if got[0].AverageLatency != 30*time.Millisecond {
		t.Fatalf("average latency = %v", got[0].AverageLatency)
	}
	if got[0].ConsecutiveFailures != 2 || got[0].LatestHealthy {
		t.Fatalf("failure state = %+v", got[0])
	}
}

func TestTrendsComparesRecentAndPreviousWindows(t *testing.T) {
	target := monitor.Target{Name: "api", URL: "https://api.example.test"}
	results := []monitor.Result{
		{Target: target, StatusCode: 200},
		{Target: target, StatusCode: 200},
		{Target: target, StatusCode: 500},
		{Target: target, Error: "timeout"},
	}
	got, err := Trends(results, 2)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 {
		t.Fatalf("len = %d", len(got))
	}
	if got[0].PreviousAvailabilityPercent != 100 || got[0].RecentAvailabilityPercent != 0 || got[0].DeltaPercent != -100 {
		t.Fatalf("trend = %+v", got[0])
	}
}

func TestTrendsRejectsInvalidWindow(t *testing.T) {
	if _, err := Trends(nil, 0); err == nil {
		t.Fatal("expected invalid window error")
	}
}

func TestTrendsOmitsTargetWithoutPreviousSample(t *testing.T) {
	got, err := Trends([]monitor.Result{{Target: monitor.Target{Name: "api"}, StatusCode: 200}}, 3)
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 0 {
		t.Fatalf("unexpected trends: %+v", got)
	}
}
