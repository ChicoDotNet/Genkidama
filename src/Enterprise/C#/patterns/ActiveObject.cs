using System;
namespace Genkidama.PatternExamples;
public static class ActiveObjectExample { public static bool Run(){var v=0;Action[] q={()=>v+=3,()=>v*=4};var before=v;foreach(var a in q)a();return before==0&&v==12;} }
