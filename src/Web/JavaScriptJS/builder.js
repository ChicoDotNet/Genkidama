class TextReportBuilder {
  constructor() {
    this.parts = [];
  }

  reset() {
    this.parts = [];
  }

  addTitle(title) {
    this.parts.push(`# ${title}`);
  }

  addSection(heading, body) {
    this.parts.push(`## ${heading}`, body);
  }

  build() {
    return this.parts.join("\n");
  }
}

class HtmlReportBuilder {
  constructor() {
    this.parts = [];
  }

  reset() {
    this.parts = [];
  }

  addTitle(title) {
    this.parts.push(`<h1>${title}</h1>`);
  }

  addSection(heading, body) {
    this.parts.push(`<h2>${heading}</h2>`, `<p>${body}</p>`);
  }

  build() {
    return this.parts.join("");
  }
}

function buildAvailabilityReport(builder) {
  builder.reset();
  builder.addTitle("Service status");
  builder.addSection("Availability", "99.95%");
  return builder.build();
}

console.log(buildAvailabilityReport(new TextReportBuilder()));
console.log("---");
console.log(buildAvailabilityReport(new HtmlReportBuilder()));
