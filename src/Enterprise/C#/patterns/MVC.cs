namespace Genkidama.PatternExamples;
public static class MvcExample { public static bool Run(){var count=0;string View()=>$"count={count}";var before=View();count++;return before=="count=0"&&View()=="count=1";} }
