package insights

import (
	"errors"
	"sort"
	"time"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
)

// Summary describes derived reliability statistics for one monitored target.
type Summary struct {
	Target              monitor.Target `json:"target"`
	Samples             int            `json:"samples"`
	Healthy             int            `json:"healthy"`
	AvailabilityPercent float64        `json:"availabilityPercent"`
	AverageLatency      time.Duration  `json:"averageLatency"`
	ConsecutiveFailures int            `json:"consecutiveFailures"`
	LatestCheckedAt     time.Time      `json:"latestCheckedAt"`
	LatestHealthy       bool           `json:"latestHealthy"`
}

// Trend compares a recent availability window with the immediately preceding window.
type Trend struct {
	Target                      monitor.Target `json:"target"`
	RecentSamples               int            `json:"recentSamples"`
	PreviousSamples             int            `json:"previousSamples"`
	RecentAvailabilityPercent   float64        `json:"recentAvailabilityPercent"`
	PreviousAvailabilityPercent float64        `json:"previousAvailabilityPercent"`
	DeltaPercent                float64        `json:"deltaPercent"`
}

type targetKey struct {
	name string
	url  string
}

// Summarize derives deterministic per-target statistics without mutating the source history.
func Summarize(results []monitor.Result) []Summary {
	grouped := group(results)
	keys := sortedKeys(grouped)
	summaries := make([]Summary, 0, len(keys))
	for _, key := range keys {
		entries := grouped[key]
		healthy := 0
		var latency time.Duration
		for _, entry := range entries {
			if entry.Healthy() {
				healthy++
			}
			latency += entry.Latency
		}
		failures := 0
		for i := len(entries) - 1; i >= 0 && !entries[i].Healthy(); i-- {
			failures++
		}
		latest := entries[len(entries)-1]
		summaries = append(summaries, Summary{
			Target:              latest.Target,
			Samples:             len(entries),
			Healthy:             healthy,
			AvailabilityPercent: percentage(healthy, len(entries)),
			AverageLatency:      latency / time.Duration(len(entries)),
			ConsecutiveFailures: failures,
			LatestCheckedAt:     latest.CheckedAt,
			LatestHealthy:       latest.Healthy(),
		})
	}
	return summaries
}

// Trends compares up to window recent samples with up to window preceding samples per target.
// Targets without a preceding sample are omitted because no comparison is possible.
func Trends(results []monitor.Result, window int) ([]Trend, error) {
	if window < 1 {
		return nil, errors.New("insights: window must be at least 1")
	}
	grouped := group(results)
	keys := sortedKeys(grouped)
	trends := make([]Trend, 0, len(keys))
	for _, key := range keys {
		entries := grouped[key]
		if len(entries) < 2 {
			continue
		}
		recentStart := max(0, len(entries)-window)
		previousStart := max(0, recentStart-window)
		recent := entries[recentStart:]
		previous := entries[previousStart:recentStart]
		if len(previous) == 0 {
			continue
		}
		recentAvailability := availability(recent)
		previousAvailability := availability(previous)
		trends = append(trends, Trend{
			Target:                      entries[len(entries)-1].Target,
			RecentSamples:               len(recent),
			PreviousSamples:             len(previous),
			RecentAvailabilityPercent:   recentAvailability,
			PreviousAvailabilityPercent: previousAvailability,
			DeltaPercent:                recentAvailability - previousAvailability,
		})
	}
	return trends, nil
}

func group(results []monitor.Result) map[targetKey][]monitor.Result {
	grouped := make(map[targetKey][]monitor.Result)
	for _, result := range results {
		key := targetKey{name: result.Target.Name, url: result.Target.URL}
		grouped[key] = append(grouped[key], result)
	}
	return grouped
}

func sortedKeys(grouped map[targetKey][]monitor.Result) []targetKey {
	keys := make([]targetKey, 0, len(grouped))
	for key := range grouped {
		keys = append(keys, key)
	}
	sort.Slice(keys, func(i, j int) bool {
		if keys[i].name == keys[j].name {
			return keys[i].url < keys[j].url
		}
		return keys[i].name < keys[j].name
	})
	return keys
}

func availability(results []monitor.Result) float64 {
	healthy := 0
	for _, result := range results {
		if result.Healthy() {
			healthy++
		}
	}
	return percentage(healthy, len(results))
}

func percentage(part, total int) float64 {
	if total == 0 {
		return 0
	}
	return float64(part) * 100 / float64(total)
}
