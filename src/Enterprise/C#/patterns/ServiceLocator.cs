using System;
using System.Collections.Generic;
namespace Genkidama.PatternExamples;
public static class ServiceLocatorExample { public static bool Run(){var s=new Dictionary<string,Func<string,string>>{{"email",v=>$"email>{v}"},{"audit",v=>$"audit>{v}"}};return s["email"]("a@example.test")=="email>a@example.test"&&s["audit"]("created")=="audit>created";} }
