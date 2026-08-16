#include <stdio.h>
#include <string.h>

typedef struct {
    char output[512];
} Report;

typedef struct ReportBuilder ReportBuilder;
struct ReportBuilder {
    Report report;
    void (*reset)(ReportBuilder *);
    void (*add_title)(ReportBuilder *, const char *);
    void (*add_section)(ReportBuilder *, const char *, const char *);
};

static void text_reset(ReportBuilder *builder) { builder->report.output[0] = '\0'; }
static void text_add_title(ReportBuilder *builder, const char *title) {
    snprintf(builder->report.output + strlen(builder->report.output),
             sizeof builder->report.output - strlen(builder->report.output), "# %s", title);
}
static void text_add_section(ReportBuilder *builder, const char *heading, const char *body) {
    snprintf(builder->report.output + strlen(builder->report.output),
             sizeof builder->report.output - strlen(builder->report.output), "\n## %s\n%s", heading, body);
}

static void html_reset(ReportBuilder *builder) { builder->report.output[0] = '\0'; }
static void html_add_title(ReportBuilder *builder, const char *title) {
    snprintf(builder->report.output + strlen(builder->report.output),
             sizeof builder->report.output - strlen(builder->report.output), "<h1>%s</h1>", title);
}
static void html_add_section(ReportBuilder *builder, const char *heading, const char *body) {
    snprintf(builder->report.output + strlen(builder->report.output),
             sizeof builder->report.output - strlen(builder->report.output), "<h2>%s</h2><p>%s</p>", heading, body);
}

static Report build_availability_report(ReportBuilder *builder) {
    builder->reset(builder);
    builder->add_title(builder, "Service status");
    builder->add_section(builder, "Availability", "99.95%");
    return builder->report;
}

int main(void) {
    ReportBuilder text = {
        .report = {.output = {0}},
        .reset = text_reset,
        .add_title = text_add_title,
        .add_section = text_add_section,
    };
    ReportBuilder html = {
        .report = {.output = {0}},
        .reset = html_reset,
        .add_title = html_add_title,
        .add_section = html_add_section,
    };
    printf("%s\n---\n%s\n", build_availability_report(&text).output, build_availability_report(&html).output);
    return 0;
}
