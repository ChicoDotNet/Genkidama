namespace Genkidama.PatternExamples;
public static class EnterpriseAdapterExample { public static bool Run(){var legacy=(Code:17,Cents:1250);var canonical=(Id:legacy.Code,Amount:legacy.Cents/100.0);return canonical==(17,12.5);} }
