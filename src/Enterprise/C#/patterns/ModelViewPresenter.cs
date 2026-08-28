namespace Genkidama.PatternExamples;
public static class ModelViewPresenterExample { public static bool Run(){var count=0;var text="";void Present(){count++;text=$"count={count}";}Present();return count==1&&text=="count=1";} }
