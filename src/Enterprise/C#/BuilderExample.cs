using System;
using System.Collections.Generic;

public interface IReportBuilder
{
    void Reset();
    void AddTitle(string title);
    void AddSection(string heading, string body);
    string Build();
}

public sealed class TextReportBuilder : IReportBuilder
{
    private readonly List<string> _parts = new();

    public void Reset() => _parts.Clear();

    public void AddTitle(string title) => _parts.Add($"# {title}");

    public void AddSection(string heading, string body)
    {
        _parts.Add($"## {heading}");
        _parts.Add(body);
    }

    public string Build() => string.Join(Environment.NewLine, _parts);
}

public sealed class HtmlReportBuilder : IReportBuilder
{
    private readonly List<string> _parts = new();

    public void Reset() => _parts.Clear();

    public void AddTitle(string title) => _parts.Add($"<h1>{title}</h1>");

    public void AddSection(string heading, string body)
    {
        _parts.Add($"<h2>{heading}</h2>");
        _parts.Add($"<p>{body}</p>");
    }

    public string Build() => string.Join(string.Empty, _parts);
}

public static class ReportDirector
{
    public static string BuildAvailabilityReport(IReportBuilder builder)
    {
        builder.Reset();
        builder.AddTitle("Service status");
        builder.AddSection("Availability", "99.95%");
        return builder.Build();
    }
}

public static class BuilderExample
{
    public static void Main()
    {
        Console.WriteLine(ReportDirector.BuildAvailabilityReport(new TextReportBuilder()));
        Console.WriteLine("---");
        Console.WriteLine(ReportDirector.BuildAvailabilityReport(new HtmlReportBuilder()));
    }
}
