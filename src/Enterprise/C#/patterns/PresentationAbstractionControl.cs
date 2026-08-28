namespace Genkidama.PatternExamples;
public static class PresentationAbstractionControlExample { public static bool Run(){string View(string n,int v)=>$"{n}:view={v}";return View("child",42)=="child:view=42"&&View("root",42)=="root:view=42";} }
