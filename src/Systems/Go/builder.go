package main

import (
	"fmt"
	"strings"
)

type ReportBuilder interface {
	Reset()
	AddTitle(string)
	AddSection(string, string)
	Build() string
}

type TextReportBuilder struct {
	parts []string
}

func (b *TextReportBuilder) Reset() {
	b.parts = nil
}

func (b *TextReportBuilder) AddTitle(title string) {
	b.parts = append(b.parts, "# "+title)
}

func (b *TextReportBuilder) AddSection(heading, body string) {
	b.parts = append(b.parts, "## "+heading, body)
}

func (b *TextReportBuilder) Build() string {
	return strings.Join(b.parts, "\n")
}

type HTMLReportBuilder struct {
	parts []string
}

func (b *HTMLReportBuilder) Reset() {
	b.parts = nil
}

func (b *HTMLReportBuilder) AddTitle(title string) {
	b.parts = append(b.parts, "<h1>"+title+"</h1>")
}

func (b *HTMLReportBuilder) AddSection(heading, body string) {
	b.parts = append(b.parts, "<h2>"+heading+"</h2>", "<p>"+body+"</p>")
}

func (b *HTMLReportBuilder) Build() string {
	return strings.Join(b.parts, "")
}

func BuildAvailabilityReport(builder ReportBuilder) string {
	builder.Reset()
	builder.AddTitle("Service status")
	builder.AddSection("Availability", "99.95%")
	return builder.Build()
}

func main() {
	fmt.Println(BuildAvailabilityReport(&TextReportBuilder{}))
	fmt.Println("---")
	fmt.Println(BuildAvailabilityReport(&HTMLReportBuilder{}))
}
