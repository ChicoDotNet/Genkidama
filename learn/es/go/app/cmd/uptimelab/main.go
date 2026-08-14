package main

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"syscall"
	"time"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/history"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/scheduler"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/web"
)

func main() {
	targets, err := parseTargets(os.Getenv("UPTIMELAB_TARGETS"))
	if err != nil {
		log.Fatal(err)
	}

	historyPath := os.Getenv("UPTIMELAB_HISTORY_FILE")
	if historyPath == "" {
		historyPath = "data/history.json"
	}
	store, err := history.NewFileStore(historyPath)
	if err != nil {
		log.Fatal(err)
	}
	historyLog, err := history.NewLog(store, 200)
	if err != nil {
		log.Fatal(err)
	}
	server, err := web.NewServerWithHistory(monitor.NewChecker(nil), targets, 4, historyLog)
	if err != nil {
		log.Fatal(err)
	}

	interval, err := parseInterval(os.Getenv("UPTIMELAB_INTERVAL"))
	if err != nil {
		log.Fatal(err)
	}
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	if interval > 0 {
		runner, _ := scheduler.New(interval, func(runCtx context.Context) error {
			_, err := server.RunChecks(runCtx)
			return err
		})
		go func() {
			if err := runner.Run(ctx); err != nil && ctx.Err() == nil {
				log.Printf("scheduled checks stopped: %v", err)
			}
		}()
	}

	address := os.Getenv("UPTIMELAB_ADDR")
	if address == "" {
		address = "127.0.0.1:8080"
	}
	httpServer := &http.Server{Addr: address, Handler: server.Handler(), ReadHeaderTimeout: 5 * time.Second}
	go func() {
		<-ctx.Done()
		shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		_ = httpServer.Shutdown(shutdownCtx)
	}()

	log.Printf("UptimeLab listening on http://%s", address)
	if err := httpServer.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		log.Fatal(err)
	}
}

func parseTargets(raw string) ([]monitor.Target, error) {
	if strings.TrimSpace(raw) == "" {
		return []monitor.Target{{Name: "Go", URL: "https://go.dev"}}, nil
	}
	parts := strings.Split(raw, ",")
	targets := make([]monitor.Target, 0, len(parts))
	for _, part := range parts {
		name, targetURL, ok := strings.Cut(part, "=")
		if !ok || strings.TrimSpace(name) == "" || strings.TrimSpace(targetURL) == "" {
			return nil, fmt.Errorf("UPTIMELAB_TARGETS entry %q must use name=https://url", part)
		}
		targets = append(targets, monitor.Target{Name: strings.TrimSpace(name), URL: strings.TrimSpace(targetURL)})
	}
	return targets, nil
}

func parseInterval(raw string) (time.Duration, error) {
	if strings.TrimSpace(raw) == "" || raw == "0" {
		return 0, nil
	}
	if seconds, err := strconv.Atoi(raw); err == nil {
		if seconds < 1 {
			return 0, fmt.Errorf("UPTIMELAB_INTERVAL must be positive seconds or a duration")
		}
		return time.Duration(seconds) * time.Second, nil
	}
	interval, err := time.ParseDuration(raw)
	if err != nil || interval <= 0 {
		return 0, fmt.Errorf("UPTIMELAB_INTERVAL must be positive seconds or a duration")
	}
	return interval, nil
}
