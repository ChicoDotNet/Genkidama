using System;
using System.Collections.Generic;
namespace Genkidama.PatternExamples;
public static class MicrokernelExample { public static bool Run(){var p=new Dictionary<string,Func<int,int>>{{"double",x=>x*2},{"square",x=>x*x}};return p["double"](4)==8&&p["square"](4)==16;} }
