namespace Genkidama.PatternExamples;
public static class LazyInitializationExample { public static bool Run(){var builds=0;string? value=null;string Get()=>value??=Build();string Build(){builds++;return "ready";}return Get()=="ready"&&Get()=="ready"&&builds==1;} }
