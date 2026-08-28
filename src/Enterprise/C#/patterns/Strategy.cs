using System;
namespace Genkidama.PatternExamples;
public static class StrategyExample { public static bool Run(){int Price(int v,Func<int,int>s)=>s(v);return Price(100,x=>x)==100&&Price(100,x=>x*80/100)==80;} }
