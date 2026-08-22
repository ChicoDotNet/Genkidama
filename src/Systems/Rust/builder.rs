trait ReportBuilder {
    fn reset(&mut self);
    fn add_title(&mut self, title: &str);
    fn add_section(&mut self, heading: &str, body: &str);
    fn build(&self) -> String;
}

#[derive(Default)]
struct TextReportBuilder {
    parts: Vec<String>,
}

impl ReportBuilder for TextReportBuilder {
    fn reset(&mut self) {
        self.parts.clear();
    }

    fn add_title(&mut self, title: &str) {
        self.parts.push(format!("# {title}"));
    }

    fn add_section(&mut self, heading: &str, body: &str) {
        self.parts.push(format!("## {heading}"));
        self.parts.push(body.to_owned());
    }

    fn build(&self) -> String {
        self.parts.join("\n")
    }
}

#[derive(Default)]
struct HtmlReportBuilder {
    parts: Vec<String>,
}

impl ReportBuilder for HtmlReportBuilder {
    fn reset(&mut self) {
        self.parts.clear();
    }

    fn add_title(&mut self, title: &str) {
        self.parts.push(format!("<h1>{title}</h1>"));
    }

    fn add_section(&mut self, heading: &str, body: &str) {
        self.parts.push(format!("<h2>{heading}</h2>"));
        self.parts.push(format!("<p>{body}</p>"));
    }

    fn build(&self) -> String {
        self.parts.concat()
    }
}

fn build_availability_report(builder: &mut dyn ReportBuilder) -> String {
    builder.reset();
    builder.add_title("Service status");
    builder.add_section("Availability", "99.95%");
    builder.build()
}

fn main() {
    println!(
        "{}",
        build_availability_report(&mut TextReportBuilder::default())
    );
    println!("---");
    println!(
        "{}",
        build_availability_report(&mut HtmlReportBuilder::default())
    );
}
