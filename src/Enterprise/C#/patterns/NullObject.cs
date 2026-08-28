namespace Genkidama.PatternExamples;
public static class NullObjectExample { private interface ILogger{string Log(string m);} private sealed class NullLogger:ILogger{public string Log(string m)=>"";} private sealed class RealLogger:ILogger{public string Log(string m)=>$"log:{m}";} public static bool Run()=>new NullLogger().Log("x")==""&&new RealLogger().Log("x")=="log:x"; }
