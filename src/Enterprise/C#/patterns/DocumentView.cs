namespace Genkidama.PatternExamples;
public static class DocumentViewExample { public static bool Run(){var d=(Title:"Final",Words:120);return $"editor:{d.Title}:{d.Words}"=="editor:Final:120"&&$"summary:{d.Title}"=="summary:Final";} }
