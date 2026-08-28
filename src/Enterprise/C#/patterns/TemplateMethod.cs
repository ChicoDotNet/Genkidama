using System;
namespace Genkidama.PatternExamples;
public static class TemplateMethodExample { public static bool Run(){string Pipe(string r,Func<string>t)=>$"{r}>{t()}>publish";return Pipe("read-csv",()=>"normalize")=="read-csv>normalize>publish";} }
