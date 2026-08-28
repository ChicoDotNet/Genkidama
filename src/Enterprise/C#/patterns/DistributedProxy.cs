namespace Genkidama.PatternExamples;
public static class DistributedProxyExample { public static bool Run(){int Remote(string sku)=>sku=="sku-1"?7:0;int Proxy(string sku)=>Remote(sku);return Proxy("sku-1")==7;} }
