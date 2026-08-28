using System;
using System.Linq;
namespace Genkidama.PatternExamples;
public static class CommandExample { public static bool Run(){Func<int,int>[] q={x=>x+50,x=>x-20};var b=q.Aggregate(100,(v,f)=>f(v));return b==130&&q[1](150)==130;} }
