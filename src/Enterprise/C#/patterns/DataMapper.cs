namespace Genkidama.PatternExamples;
public static class DataMapperExample { public static bool Run(){var p=(Id:8,Name:"Grace");var row=(Key:$"person:{p.Id}",p.Name);return row.Key=="person:8"&&row.Name=="Grace";} }
