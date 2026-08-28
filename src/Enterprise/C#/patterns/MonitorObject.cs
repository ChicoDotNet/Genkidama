namespace Genkidama.PatternExamples;
public static class MonitorObjectExample { private sealed class MonitoredCounter{private readonly object _gate=new();private int _value;public void Add(int x){lock(_gate)_value+=x;}public int Value{get{lock(_gate)return _value;}}} public static bool Run(){var c=new MonitoredCounter();c.Add(2);c.Add(3);return c.Value==5;} }
