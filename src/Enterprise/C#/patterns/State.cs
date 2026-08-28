namespace Genkidama.PatternExamples;
public static class StateExample { public static bool Run(){string T(string s,string a)=>s=="locked"&&a=="unlock"?"unlocked":s=="unlocked"&&a=="lock"?"locked":s;return T(T("locked","unlock"),"lock")=="locked";} }
