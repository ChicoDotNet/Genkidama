using System;
using System.Linq;
namespace Genkidama.PatternExamples;
public static class MessageBusExample { public static bool Run(){Func<string,int,string>[] h={(t,i)=>$"audit:{t}:{i}",(t,i)=>$"billing:{t}:{i}"};return string.Join('>',h.Select(x=>x("order-created",42)))=="audit:order-created:42>billing:order-created:42";} }
