using System;
using System.Collections.Generic;
namespace Genkidama.PatternExamples;
public static class BrokerExample { public static bool Run(){var s=new Dictionary<string,Func<string,string>>{{"inventory",k=>$"inventory:{k}=7"},{"customer",k=>$"customer:{k}=active"}};return s["inventory"]("sku-1")=="inventory:sku-1=7"&&s["customer"]("17")=="customer:17=active";} }
