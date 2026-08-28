using System;
namespace Genkidama.PatternExamples;
public static class DependencyInjectionExample { public static bool Run(){string Service(Func<string>clock)=>$"at:{clock()}";return Service(()=>"10:00")=="at:10:00";} }
