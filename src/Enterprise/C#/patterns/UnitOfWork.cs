using System.Collections.Generic;
using System.Linq;
namespace Genkidama.PatternExamples;
public static class UnitOfWorkExample { public static bool Run(){var store=new List<int>();var pending=new List<int>{2,3};store.AddRange(pending);pending.Clear();return store.SequenceEqual(new[]{2,3})&&pending.Count==0;} }
