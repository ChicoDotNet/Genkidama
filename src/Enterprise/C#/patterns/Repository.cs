using System.Linq;
namespace Genkidama.PatternExamples;
public static class RepositoryExample { public static bool Run()=>new[]{(Id:1,Name:"Ada"),(Id:2,Name:"Grace")}.Single(x=>x.Id==2).Name=="Grace"; }
