package main

import (
	"log"
	"net/http"
	"os"
	"strings"

	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/monitor"
	"github.com/ChicoDotNet/Genkidama/learn/es/go/app/web"
)

func main() {
	targets := parseTargets(os.Getenv("UPTIMELAB_TARGETS"))
	server, err := web.NewServer(monitor.NewChecker(nil), targets, 4)
	if err != nil {
		log.Fatal(err)
	}
	address := os.Getenv("UPTIMELAB_ADDR")
	if address == "" {
		address = "127.0.0.1:8080"
	}
	log.Printf("UptimeLab listening on http://%s", address)
	if err := http.ListenAndServe(address, server.Handler()); err != nil {
		log.Fatal(err)
	}
}

func parseTargets(raw string) []monitor.Target {
	if strings.TrimSpace(raw) == "" {
		return []monitor.Target{{Name: "Go", URL: "https://go.dev"}}
	}
	parts := strings.Split(raw, ",")
	targets := make([]monitor.Target, 0, len(parts))
	for _, part := range parts {
		name, targetURL, ok := strings.Cut(part, "=")
		if !ok {
			continue
		}
		targets = append(targets, monitor.Target{Name: strings.TrimSpace(name), URL: strings.TrimSpace(targetURL)})
	}
	return targets
}
