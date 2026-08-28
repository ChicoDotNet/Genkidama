using System.Collections.Generic;
namespace Genkidama.PatternExamples;
public static class ObjectPoolExample { public static bool Run(){var pool=new Stack<int>(new[]{1,2});var x=pool.Pop();pool.Push(x);return pool.Count==2&&pool.Contains(x);} }
