namespace Genkidama.PatternExamples;
public static class MvvmExample { public static bool Run(){var amount=10;string Text()=>$"${amount}.00";var before=Text();amount+=5;return before=="$10.00"&&Text()=="$15.00";} }
