using System.Collections.Generic;
using System.Linq;
namespace Genkidama.PatternExamples;
public static class IteratorExample { public static bool Run(){using var it=new List<int>{10,20,30}.GetEnumerator();var seen=new List<int>();while(it.MoveNext())seen.Add(it.Current);return seen.SequenceEqual(new[]{10,20,30})&&!it.MoveNext();} }
