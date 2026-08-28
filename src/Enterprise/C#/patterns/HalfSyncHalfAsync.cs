using System.Linq;
namespace Genkidama.PatternExamples;
public static class HalfSyncHalfAsyncExample { public static bool Run()=>string.Join('>',new[]{"job-1","job-2","job-3"}.Select(j=>$"done:{j}"))=="done:job-1>done:job-2>done:job-3"; }
