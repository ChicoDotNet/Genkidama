namespace Genkidama.PatternExamples;
public static class EnterpriseBridgeExample { public static bool Run(){string Send(string t,string k,string m)=>$"{t}>{k}:{m}";return Send("kafka","ALERT","disk")=="kafka>ALERT:disk"&&Send("queue","REMINDER","backup")=="queue>REMINDER:backup";} }
