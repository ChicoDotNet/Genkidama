#!/usr/bin/env python3
from __future__ import annotations

from pathlib import Path


def replace_once(path: str, old: str, new: str) -> None:
    target = Path(path)
    text = target.read_text(encoding="utf-8")
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"{path}: expected exactly one match, found {count}")
    target.write_text(text.replace(old, new, 1), encoding="utf-8")


def main() -> int:
    replace_once(
        "eng/ci/adapters/dart_contracts.py",
        '''def patterns() -> None:\n    sweep = ROOT / "src/Web/Dart/pattern_sweep.dart"\n    mediator = ROOT / "src/Web/Dart/mediator.dart"\n    sources = [str(sweep), str(mediator)]\n    run(["dart", "format", "--output=none", "--set-exit-if-changed", *sources])\n    run(["dart", "analyze", "--fatal-infos", "--fatal-warnings", *sources])\n    require(\n        last_line(run(["dart", "run", str(mediator)], capture=True)) == "Dart Mediator: passed",\n        "Dart Mediator canonical output mismatch",\n    )\n    require(\n        last_line(run(["dart", "run", str(sweep)], capture=True)) == "Dart pattern sweep: 39/39 examples passed",\n        "Dart aggregate output mismatch",\n    )\n''',
        '''def patterns() -> None:\n    sweep = ROOT / "src/Web/Dart/pattern_sweep.dart"\n    mediator = ROOT / "src/Web/Dart/mediator.dart"\n    memento = ROOT / "src/Web/Dart/memento.dart"\n    sources = [str(sweep), str(mediator), str(memento)]\n    run(["dart", "format", "--output=none", "--set-exit-if-changed", *sources])\n    run(["dart", "analyze", "--fatal-infos", "--fatal-warnings", *sources])\n    require(\n        last_line(run(["dart", "run", str(mediator)], capture=True)) == "Dart Mediator: passed",\n        "Dart Mediator canonical output mismatch",\n    )\n    require(\n        last_line(run(["dart", "run", str(memento)], capture=True)) == "Dart Memento: passed",\n        "Dart Memento canonical output mismatch",\n    )\n    require(\n        last_line(run(["dart", "run", str(sweep)], capture=True)) == "Dart pattern sweep: 39/39 examples passed",\n        "Dart aggregate output mismatch",\n    )\n''',
    )

    replace_once(
        "eng/ci/adapters/platform_patterns.py",
        "import os\nimport sys\n",
        "import os\nimport sqlite3\nimport sys\n",
    )
    replace_once(
        "eng/ci/adapters/platform_patterns.py",
        '    "mediator.asm": "Assembly Mediator: passed",\n',
        '    "mediator.asm": "Assembly Mediator: passed",\n    "memento.asm": "Assembly Memento: passed",\n',
    )
    replace_once(
        "eng/ci/adapters/platform_patterns.py",
        "\n\ndef validate_portable() -> None:\n",
        '''\n\ndef validate_sql_memento() -> None:\n    source = (dc.ROOT / "src/Data/SQL/memento.sql").read_text(encoding="utf-8")\n    connection = sqlite3.connect(":memory:")\n    try:\n        row = connection.execute(source).fetchone()\n    finally:\n        connection.close()\n    dc.require(row == ("SQL Memento: passed",), f"SQL Memento contract failed: {row!r}")\n    print("PASS SQL memento.sql", flush=True)\n\n\ndef validate_portable() -> None:\n''',
    )
    replace_once(
        "eng/ci/adapters/platform_patterns.py",
        "    validate_assembly()\n\n    godot =",
        "    validate_assembly()\n    validate_sql_memento()\n\n    godot =",
    )
    replace_once(
        "eng/ci/adapters/platform_patterns.py",
        '    mediator_output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/mediator.gd")], capture=True)\n    dc.require("GDScript Mediator: passed" in mediator_output.splitlines(), "GDScript Mediator canonical output mismatch")\n',
        '''    mediator_output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/mediator.gd")], capture=True)\n    dc.require("GDScript Mediator: passed" in mediator_output.splitlines(), "GDScript Mediator canonical output mismatch")\n    memento_output = dc.run([godot, "--headless", "--script", str(dc.ROOT / "src/Niche/GDScript/memento.gd")], capture=True)\n    dc.require("GDScript Memento: passed" in memento_output.splitlines(), "GDScript Memento contract failed")\n''',
    )
    replace_once(
        "eng/ci/adapters/platform_patterns.py",
        '    mediator_output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/mediator.py")], capture=True)\n    dc.require(dc.last_line(mediator_output) == "MicroPython Mediator: passed", "MicroPython Mediator canonical output mismatch")\n',
        '''    mediator_output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/mediator.py")], capture=True)\n    dc.require(dc.last_line(mediator_output) == "MicroPython Mediator: passed", "MicroPython Mediator canonical output mismatch")\n    memento_output = dc.run([micropython, str(dc.ROOT / "src/Other/MicroPython/memento.py")], capture=True)\n    dc.require(dc.last_line(memento_output) == "MicroPython Memento: passed", "MicroPython Memento contract failed")\n''',
    )
    replace_once(
        "eng/ci/adapters/platform_patterns.py",
        '    mediator_output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/mediator.rock")], capture=True)\n    dc.require(dc.last_line(mediator_output) == "Rockstar Mediator: passed", "Rockstar Mediator canonical output mismatch")\n',
        '''    mediator_output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/mediator.rock")], capture=True)\n    dc.require(dc.last_line(mediator_output) == "Rockstar Mediator: passed", "Rockstar Mediator canonical output mismatch")\n    memento_output = dc.run([rockstar, str(dc.ROOT / "src/Other/Rockstar/memento.rock")], capture=True)\n    dc.require(dc.last_line(memento_output) == "Rockstar Memento: passed", "Rockstar Memento contract failed")\n''',
    )

    replace_once(
        "eng/ci/adapters/platform_source_contracts.py",
        '    ]:\n        require(vba, pattern, label)\n\n    delphi = (ROOT / "src/Enterprise/Delphi/Example1.pas").read_text(encoding="utf-8")\n',
        '''    ]:\n        require(vba, pattern, label)\n\n    vba_memento = (ROOT / "src/Shell/VBA/memento.bas").read_text(encoding="utf-8")\n    for pattern, label in [\n        (r"^Option Explicit$", "VBA Memento Option Explicit"),\n        (r"Private\\s+Type\\s+MementoSnapshot.*Title\\s+As\\s+String.*Tags\\s+As\\s+String.*End\\s+Type", "VBA Memento snapshot value"),\n        (r"Private\\s+Type\\s+Document.*Title\\s+As\\s+String.*Tags\\s+As\\s+String.*End\\s+Type", "VBA Memento originator value"),\n        (r"Function\\s+SaveMemento\\s*\\(ByRef\\s+originator\\s+As\\s+Document\\)\\s+As\\s+MementoSnapshot.*snapshot\\.Title\\s*=\\s*originator\\.Title.*snapshot\\.Tags\\s*=\\s*originator\\.Tags", "VBA originator owns capture"),\n        (r"Sub\\s+RestoreMemento\\s*\\(ByRef\\s+originator\\s+As\\s+Document,\\s*ByRef\\s+snapshot\\s+As\\s+MementoSnapshot\\).*originator\\.Title\\s*=\\s*snapshot\\.Title.*originator\\.Tags\\s*=\\s*snapshot\\.Tags", "VBA originator owns restore"),\n        (r"caretakerSnapshot\\s*=\\s*SaveMemento\\(originator\\).*originator\\.Title\\s*=\\s*\\\"published\\\".*RestoreMemento\\s+originator,\\s*caretakerSnapshot.*Debug\\.Assert\\s+originator\\.Title\\s*=\\s*\\\"draft\\\".*Debug\\.Assert\\s+caretakerSnapshot\\.Title\\s*=\\s*\\\"draft\\\"", "VBA Memento mutation restore and snapshot independence"),\n    ]:\n        require(vba_memento, pattern, label)\n\n    delphi = (ROOT / "src/Enterprise/Delphi/Example1.pas").read_text(encoding="utf-8")\n''',
    )
    replace_once(
        "eng/ci/adapters/platform_source_contracts.py",
        '    ]:\n        require(delphi, pattern, label)\n\n    vba_mediator = (ROOT / "src/Shell/VBA/MediatorExample.bas").read_text(encoding="utf-8")\n',
        '''    ]:\n        require(delphi, pattern, label)\n\n    delphi_memento = (ROOT / "src/Enterprise/Delphi/Memento.pas").read_text(encoding="utf-8")\n    for pattern, label in [\n        (r"TMementoSnapshot\\s*=\\s*record.*Title:\\s*string;.*Tags:\\s*string;.*end;", "Delphi Memento snapshot record"),\n        (r"TDocument\\s*=\\s*class.*function\\s+SaveMemento:\\s*TMementoSnapshot;.*procedure\\s+RestoreMemento\\(const\\s+Snapshot:\\s*TMementoSnapshot\\);", "Delphi originator capture/restore API"),\n        (r"function\\s+TDocument\\.SaveMemento:\\s*TMementoSnapshot;.*Result\\.Title\\s*:=\\s*FTitle;.*Result\\.Tags\\s*:=\\s*FTags;", "Delphi originator owns capture"),\n        (r"procedure\\s+TDocument\\.RestoreMemento\\(const\\s+Snapshot:\\s*TMementoSnapshot\\);.*FTitle\\s*:=\\s*Snapshot\\.Title;.*FTags\\s*:=\\s*Snapshot\\.Tags;", "Delphi originator owns restore"),\n        (r"CaretakerSnapshot\\s*:=\\s*Originator\\.SaveMemento;.*Originator\\.Title\\s*:=\\s*'published';.*Originator\\.RestoreMemento\\(CaretakerSnapshot\\);.*Originator\\.Title\\s*<>\\s*'draft'.*CaretakerSnapshot\\.Title\\s*<>\\s*'draft'", "Delphi Memento mutation restore and snapshot independence"),\n    ]:\n        require(delphi_memento, pattern, label)\n\n    vba_mediator = (ROOT / "src/Shell/VBA/MediatorExample.bas").read_text(encoding="utf-8")\n''',
    )
    replace_once(
        "eng/ci/adapters/platform_source_contracts.py",
        '    print("VBA Abstract Factory source contract: OK")\n    print("Delphi Abstract Factory source contract: OK")\n    print("VBA Mediator source contract: OK")\n    print("Delphi Mediator source contract: OK")\n',
        '    print("VBA Abstract Factory source contract: OK")\n    print("VBA Memento source contract: OK")\n    print("Delphi Abstract Factory source contract: OK")\n    print("Delphi Memento source contract: OK")\n    print("VBA Mediator source contract: OK")\n    print("Delphi Mediator source contract: OK")\n',
    )

    replace_once(
        "src/DataScience/Julia/pattern_sweep.jl",
        'include("patterns/mediator.jl")\n',
        'include("patterns/mediator.jl")\ninclude(joinpath(@__DIR__, "memento.jl"))\n',
    )
    replace_once(
        "src/DataScience/Julia/pattern_sweep.jl",
        '''# Memento\nstruct EditorMemento; state::String; end\nmutable struct Editor; state::String; end\nsave(editor::Editor) = EditorMemento(editor.state)\nrestore!(editor::Editor, memento::EditorMemento) = (editor.state = memento.state)\nfunction memento_pattern()\n    editor = Editor("draft"); snapshot = save(editor); editor.state = "published"\n    must(editor.state == "published"); restore!(editor, snapshot); must(editor.state == "draft")\nend\n''',
        '# Memento is delegated to the individually addressable canonical included above.\n',
    )
    replace_once(
        "src/DataScience/Julia/pattern_sweep.jl",
        'command_pattern, interpreter_pattern, mediator_pattern, memento_pattern, observer_pattern',
        'command_pattern, interpreter_pattern, mediator_pattern, verify_memento_canonical, observer_pattern',
    )

    replace_once(
        "src/Functional/Haskell/PatternSweep.hs",
        'import Data.Maybe (fromMaybe)\nimport System.Process (readProcess)\n',
        'import Data.Maybe (fromMaybe)\nimport qualified Memento as Memento\nimport System.Process (readProcess)\n',
    )
    replace_once(
        "src/Functional/Haskell/PatternSweep.hs",
        '''-- Memento\nnewtype EditorMemento = EditorMemento String\nrestoreEditor :: EditorMemento -> String\nrestoreEditor (EditorMemento s) = s\nmementoCase :: Bool\nmementoCase = let original="draft"; snapshot=EditorMemento original; changed="published" in changed=="published" && restoreEditor snapshot=="draft"\n\n''',
        '',
    )
    replace_once(
        "src/Functional/Haskell/PatternSweep.hs",
        'pureCases = [ commandCase, interpreterCase, mementoCase, observerCase,',
        'pureCases = [ commandCase, interpreterCase, Memento.verifyMementoCanonical, observerCase,',
    )

    replace_once(
        "src/Niche/Crystal/pattern_sweep.cr",
        'require "./mediator"\nrequire "./iterator"\n',
        'require "./mediator"\nrequire "./iterator"\nrequire "./memento"\n',
    )
    replace_once(
        "src/Niche/Crystal/pattern_sweep.cr",
        '''# Memento\nrecord EditorMemento, state : String\n\nclass Editor\n  property state : String\n\n  def initialize(@state : String); end\n\n  def save : EditorMemento\n    EditorMemento.new(@state)\n  end\n\n  def restore(m : EditorMemento)\n    @state = m.state\n  end\nend\n\ndef memento_pattern\n  e = Editor.new("draft"); snapshot = e.save; e.state = "published"\n  must(e.state == "published"); e.restore(snapshot); must(e.state == "draft")\nend\n\n''',
        '# Memento is delegated to the individually addressable canonical required above.\n\n',
    )
    replace_once(
        "src/Niche/Crystal/pattern_sweep.cr",
        'command_pattern; interpreter_pattern; iterator_pattern; mediator_pattern; memento_pattern; observer_pattern;',
        'command_pattern; interpreter_pattern; iterator_pattern; mediator_pattern; verify_memento_canonical; observer_pattern;',
    )

    replace_once(
        "src/Scripting/PythonPY/pattern_sweep.py",
        'from mediator import verify_mediator\n',
        'from mediator import verify_mediator\nfrom memento import verify_memento\n',
    )
    replace_once(
        "src/Scripting/PythonPY/pattern_sweep.py",
        '''def memento() -> None:\n    state = {"text": "draft"}\n    snapshot = state.copy()\n    state["text"] = "edited"\n    state.clear()\n    state.update(snapshot)\n    assert state["text"] == "draft"\n\n\n''',
        '',
    )
    replace_once(
        "src/Scripting/PythonPY/pattern_sweep.py",
        '    memento,\n',
        '    verify_memento,\n',
    )

    replace_once(
        "src/Systems/Objective-C/pattern_sweep.m",
        '#undef GENKIDAMA_MEDIATOR_EMBEDDED\n\nstatic void must',
        '#undef GENKIDAMA_MEDIATOR_EMBEDDED\n\n#import "memento.m"\n\nstatic void must',
    )
    replace_once(
        "src/Systems/Objective-C/pattern_sweep.m",
        '''// Memento\n@interface Editor : NSObject { NSString *_state; }\n- (instancetype)initWithState:(NSString *)state; - (NSString *)save; - (void)restore:(NSString *)snapshot; - (void)setState:(NSString *)state; - (NSString *)state;\n@end\n@implementation Editor\n- (instancetype)initWithState:(NSString *)state { if ((self = [super init])) _state = [state copy]; return self; }\n- (NSString *)save { return [_state copy]; }\n- (void)restore:(NSString *)snapshot { _state = [snapshot copy]; }\n- (void)setState:(NSString *)state { _state = [state copy]; }\n- (NSString *)state { return _state; }\n@end\nstatic BOOL mementoPattern(void) { Editor *e = [[Editor alloc] initWithState:@"draft"]; NSString *snapshot = [e save]; [e setState:@"published"]; BOOL changed = [[e state] isEqualToString:@"published"]; [e restore:snapshot]; return changed && [[e state] isEqualToString:@"draft"]; }\n\n''',
        '',
    )
    replace_once(
        "src/Systems/Objective-C/pattern_sweep.m",
        'commandPattern, interpreterPattern, iteratorPattern, mediatorPattern, mementoPattern, observerPattern,',
        'commandPattern, interpreterPattern, iteratorPattern, mediatorPattern, verifyMementoCanonical, observerPattern,',
    )

    replace_once(
        "src/Systems/Zig/pattern_sweep.zig",
        'const iterator_example = @import("iterator.zig");\n',
        'const iterator_example = @import("iterator.zig");\nconst memento = @import("memento.zig");\n',
    )
    replace_once(
        "src/Systems/Zig/pattern_sweep.zig",
        '''// Memento\nconst Editor = struct { state: enum { draft, published } };\nfn mementoPattern() bool {\n    var editor = Editor{ .state = .draft };\n    const snapshot = editor;\n    editor.state = .published;\n    if (editor.state != .published) return false;\n    editor = snapshot;\n    return editor.state == .draft;\n}\n\n''',
        '// Memento is delegated to the individually addressable canonical imported above.\n\n',
    )
    replace_once(
        "src/Systems/Zig/pattern_sweep.zig",
        'mementoPattern,',
        'memento.verifyMementoCanonical,',
    )

    replace_once(
        "src/Web/Dart/pattern_sweep.dart",
        "import 'iterator.dart' as iterator_example;\n",
        "import 'iterator.dart' as iterator_example;\nimport 'memento.dart' show verifyMementoCanonical;\n",
    )
    replace_once(
        "src/Web/Dart/pattern_sweep.dart",
        '''// Memento\nclass EditorMemento {\n  const EditorMemento(this.state);\n  final String state;\n}\n\nclass Editor {\n  Editor(this.state);\n  String state;\n  EditorMemento save() => EditorMemento(state);\n  void restore(EditorMemento m) => state = m.state;\n}\n\nvoid mementoPattern() {\n  final e = Editor('draft');\n  final snapshot = e.save();\n  e.state = 'published';\n  check(e.state == 'published');\n  e.restore(snapshot);\n  check(e.state == 'draft');\n}\n\n''',
        '// Memento is implemented canonically in memento.dart; this sweep only orchestrates it.\n\n',
    )
    replace_once(
        "src/Web/Dart/pattern_sweep.dart",
        '    mementoPattern,\n',
        '    verifyMementoCanonical,\n',
    )

    required = [
        "src/DataScience/Julia/memento.jl",
        "src/Functional/Haskell/Memento.hs",
        "src/Niche/Crystal/memento.cr",
        "src/Scripting/PythonPY/memento.py",
        "src/Systems/Objective-C/memento.m",
        "src/Systems/Zig/memento.zig",
        "src/Web/Dart/memento.dart",
    ]
    missing = [path for path in required if not Path(path).is_file()]
    if missing:
        raise SystemExit(f"missing canonical Memento files: {missing}")

    print("Memento reconciliation transforms: OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
