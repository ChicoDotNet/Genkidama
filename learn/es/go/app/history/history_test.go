package history

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
)

type failingStore struct {
	entries []monitor.Result
}

func (s *failingStore) Load() ([]monitor.Result, error) {
	return append([]monitor.Result(nil), s.entries...), nil
}

func (s *failingStore) Save([]monitor.Result) error {
	return errors.New("disk unavailable")
}

type memoryStore struct {
	entries []monitor.Result
}

func (s *memoryStore) Load() ([]monitor.Result, error) {
	return append([]monitor.Result(nil), s.entries...), nil
}

func (s *memoryStore) Save(value []monitor.Result) error {
	s.entries = append([]monitor.Result(nil), value...)
	return nil
}

func TestFileStoreRoundTrip(t *testing.T) {
	path := filepath.Join(t.TempDir(), "history.json")
	store, err := NewFileStore(path)
	if err != nil {
		t.Fatal(err)
	}
	input := []monitor.Result{{
		Target:     monitor.Target{Name: "api", URL: "https://example.test"},
		StatusCode: 200,
		CheckedAt:  time.Date(2026, 8, 14, 12, 0, 0, 0, time.UTC),
	}}
	if err := store.Save(input); err != nil {
		t.Fatal(err)
	}
	got, err := store.Load()
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 1 || got[0].Target.Name != "api" {
		t.Fatalf("unexpected history: %+v", got)
	}
}

func TestFileStoreMissingIsEmpty(t *testing.T) {
	store, _ := NewFileStore(filepath.Join(t.TempDir(), "missing.json"))
	got, err := store.Load()
	if err != nil {
		t.Fatal(err)
	}
	if len(got) != 0 {
		t.Fatalf("expected empty history: %+v", got)
	}
}

func TestFileStoreRejectsCorruptJSON(t *testing.T) {
	path := filepath.Join(t.TempDir(), "history.json")
	if err := os.WriteFile(path, []byte("not-json"), 0o600); err != nil {
		t.Fatal(err)
	}
	store, _ := NewFileStore(path)
	if _, err := store.Load(); err == nil {
		t.Fatal("expected decode error")
	}
}

func TestAppendKeepsVisibleStateWhenPersistenceFails(t *testing.T) {
	initial := monitor.Result{Target: monitor.Target{Name: "old", URL: "https://old.test"}}
	log, err := NewLog(&failingStore{entries: []monitor.Result{initial}}, 10)
	if err != nil {
		t.Fatal(err)
	}
	if err := log.Append([]monitor.Result{{Target: monitor.Target{Name: "new", URL: "https://new.test"}}}); err == nil {
		t.Fatal("expected persistence failure")
	}
	got := log.Entries()
	if len(got) != 1 || got[0].Target.Name != "old" {
		t.Fatalf("visible history changed: %+v", got)
	}
}

func TestAppendKeepsOnlyNewestEntries(t *testing.T) {
	store := &memoryStore{}
	log, _ := NewLog(store, 2)
	_ = log.Append([]monitor.Result{
		{Target: monitor.Target{Name: "one"}},
		{Target: monitor.Target{Name: "two"}},
		{Target: monitor.Target{Name: "three"}},
	})
	got := log.Entries()
	if len(got) != 2 || got[0].Target.Name != "two" || got[1].Target.Name != "three" {
		t.Fatalf("unexpected bounded history: %+v", got)
	}
}
