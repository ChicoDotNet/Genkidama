using System.Linq;
namespace Genkidama.PatternExamples;
public static class LeaderFollowersExample { public static bool Run(){var w=new[]{"worker-1","worker-2","worker-3"};var e=new[]{"a","b","c"};var handled=e.Select((x,i)=>$"{w[i%w.Length]}:{x}");return string.Join('>',handled)=="worker-1:a>worker-2:b>worker-3:c"&&w[e.Length%w.Length]=="worker-1";} }
