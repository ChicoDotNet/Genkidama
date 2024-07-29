# Abstract Factory
create_report <- function(report_type) {
  if (report_type == "pdf") {
    return(list(generate = generate_pdf_report))
  } else if (report_type == "html") {
    return(list(generate = generate_html_report))
  }
}

# Concrete Implementations
generate_pdf_report <- function() {
  cat("Generating PDF report\n")
}

generate_html_report <- function() {
  cat("Generating HTML report\n")
}

# Usage
report <- create_report("pdf")
report$generate()

report <- create_report("html")
report$generate()
