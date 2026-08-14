package history

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
)

// Store persists and restores uptime history snapshots.
type Store interface {
	Load() ([]monitor.Result, error)
	Save([]monitor.Result) error
}

// FileStore stores history as JSON using a temporary file and rename.
type FileStore struct {
	path string
}

// NewFileStore creates a JSON-backed history store at path.
func NewFileStore(path string) (*FileStore, error) {
	if path == "" {
		return nil, errors.New("history: path is required")
	}
	return &FileStore{path: path}, nil
}

// Load returns persisted history. A missing file is an empty first-run history.
func (s *FileStore) Load() ([]monitor.Result, error) {
	data, err := os.ReadFile(s.path)
	if errors.Is(err, os.ErrNotExist) {
		return []monitor.Result{}, nil
	}
	if err != nil {
		return nil, fmt.Errorf("history: read %s: %w", s.path, err)
	}
	var results []monitor.Result
	if err := json.Unmarshal(data, &results); err != nil {
		return nil, fmt.Errorf("history: decode %s: %w", s.path, err)
	}
	return results, nil
}

// Save replaces the persisted history snapshot through a same-directory temporary file.
func (s *FileStore) Save(results []monitor.Result) error {
	data, err := json.MarshalIndent(results, "", "  ")
	if err != nil {
		return fmt.Errorf("history: encode: %w", err)
	}
	if err := os.MkdirAll(filepath.Dir(s.path), 0o755); err != nil {
		return fmt.Errorf("history: create directory: %w", err)
	}
	temp := s.path + ".tmp"
	if err := os.WriteFile(temp, append(data, '\n'), 0o600); err != nil {
		return fmt.Errorf("history: write temp: %w", err)
	}
	if err := os.Rename(temp, s.path); err != nil {
		_ = os.Remove(temp)
		return fmt.Errorf("history: replace: %w", err)
	}
	return nil
}

// Log owns an in-memory bounded history synchronized with a Store.
type Log struct {
	mu      sync.RWMutex
	store   Store
	entries []monitor.Result
	limit   int
}

// NewLog restores a bounded history from store.
func NewLog(store Store, limit int) (*Log, error) {
	if store == nil {
		return nil, errors.New("history: store is required")
	}
	if limit < 1 {
		return nil, errors.New("history: limit must be at least 1")
	}
	entries, err := store.Load()
	if err != nil {
		return nil, err
	}
	if len(entries) > limit {
		entries = append([]monitor.Result(nil), entries[len(entries)-limit:]...)
	}
	return &Log{store: store, entries: append([]monitor.Result(nil), entries...), limit: limit}, nil
}

// Entries returns a snapshot ordered from oldest to newest.
func (l *Log) Entries() []monitor.Result {
	l.mu.RLock()
	defer l.mu.RUnlock()
	return append([]monitor.Result(nil), l.entries...)
}

// Append persists a candidate snapshot before making it visible in memory.
func (l *Log) Append(results []monitor.Result) error {
	l.mu.Lock()
	defer l.mu.Unlock()
	candidate := append(append([]monitor.Result(nil), l.entries...), results...)
	if len(candidate) > l.limit {
		candidate = append([]monitor.Result(nil), candidate[len(candidate)-l.limit:]...)
	}
	if err := l.store.Save(candidate); err != nil {
		return err
	}
	l.entries = candidate
	return nil
}
