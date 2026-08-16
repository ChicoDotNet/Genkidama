interface ReportBuilder {
  reset(): void;
  addTitle(title: string): void;
  addSection(heading: string, body: string): void;
  build(): string;
}

class TextReportBuilder implements ReportBuilder {
  private parts: string[] = [];

  reset(): void {
    this.parts = [];
  }

  addTitle(title: string): void {
    this.parts.push(`# ${title}`);
  }

  addSection(heading: string, body: string): void {
    this.parts.push(`## ${heading}`, body);
  }

  build(): string {
    return this.parts.join("\n");
  }
}

class HtmlReportBuilder implements ReportBuilder {
  private parts: string[] = [];

  reset(): void {
    this.parts = [];
  }

  addTitle(title: string): void {
    this.parts.push(`<h1>${title}</h1>`);
  }

  addSection(heading: string, body: string): void {
    this.parts.push(`<h2>${heading}</h2>`, `<p>${body}</p>`);
  }

  build(): string {
    return this.parts.join("");
  }
}

function buildAvailabilityReport(builder: ReportBuilder): string {
  builder.reset();
  builder.addTitle("Service status");
  builder.addSection("Availability", "99.95%");
  return builder.build();
}

console.log(buildAvailabilityReport(new TextReportBuilder()));
console.log("---");
console.log(buildAvailabilityReport(new HtmlReportBuilder()));
