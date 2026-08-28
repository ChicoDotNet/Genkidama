using System.Collections.Generic;
namespace Genkidama.PatternExamples;
public static class PeerToPeerExample { public static bool Run(){var inbox=new List<string>();void Send(string f,string t,string d)=>inbox.Add($"{f}>{t}:{d}");Send("peer-a","peer-b","block-42");Send("peer-a","peer-c","block-42");return string.Join('>',inbox)=="peer-a>peer-b:block-42>peer-a>peer-c:block-42";} }
