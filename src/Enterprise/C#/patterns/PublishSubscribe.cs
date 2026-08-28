using System;
using System.Linq;
namespace Genkidama.PatternExamples;
public static class PublishSubscribeExample { public static bool Run(){Func<int,string>[] s={i=>$"warehouse:{i}",i=>$"analytics:{i}"};return string.Join('>',s.Select(x=>x(51)))=="warehouse:51>analytics:51";} }
