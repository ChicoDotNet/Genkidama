defmodule ReportFactory do
  def generate(:pdf), do: PDFReport.generate()
  def generate(:html), do: HTMLReport.generate()
end

defmodule PDFReport do
  def generate() do
    IO.puts "Generating PDF report"
  end
end

defmodule HTMLReport do
  def generate() do
    IO.puts "Generating HTML report"
  end
end

# Usage
ReportFactory.generate(:pdf)
ReportFactory.generate(:html)
