namespace Genkidama.PatternExamples;
public static class MementoExample { public static bool Run(){var state="draft";var snapshot=state;state="published";state=snapshot;return state=="draft";} }
