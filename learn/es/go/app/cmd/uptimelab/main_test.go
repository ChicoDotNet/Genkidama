package main

import (
	"testing"
	"time"
)

func TestParseInterval(t *testing.T) {
	tests := []struct {
		raw  string
		want time.Duration
		ok   bool
	}{
		{raw: "", want: 0, ok: true},
		{raw: "0", want: 0, ok: true},
		{raw: "30", want: 30 * time.Second, ok: true},
		{raw: "2m", want: 2 * time.Minute, ok: true},
		{raw: "-1", want: 0, ok: false},
		{raw: "wat", want: 0, ok: false},
	}
	for _, tt := range tests {
		got, err := parseInterval(tt.raw)
		if (err == nil) != tt.ok || got != tt.want {
			t.Fatalf("parseInterval(%q) = (%v, %v)", tt.raw, got, err)
		}
	}
}
