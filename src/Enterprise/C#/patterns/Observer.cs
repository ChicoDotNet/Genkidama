using System;
using System.Linq;
namespace Genkidama.PatternExamples;
public static class ObserverExample { public static bool Run(){Func<int,string>[] obs={i=>$"audit:{i}",i=>$"dashboard:{i}"};return string.Join('>',obs.Select(o=>o(42)))=="audit:42>dashboard:42";} }
