using System.Collections.Generic;
namespace Genkidama.PatternExamples;
public static class MediatorExample { public static bool Run(){var e=new List<string>();void Notify(string s,string x){if(s=="button"&&x=="click")e.Add("panel.refresh");if(s=="panel"&&x=="loaded")e.Add("button.enable");}Notify("button","click");Notify("panel","loaded");return string.Join('>',e)=="panel.refresh>button.enable";} }
