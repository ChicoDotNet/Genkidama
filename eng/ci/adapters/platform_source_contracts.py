#!/usr/bin/env python3
from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]


def require(text: str, pattern: str, label: str) -> None:
    if re.search(pattern, text, flags=re.IGNORECASE | re.MULTILINE | re.DOTALL) is None:
        raise SystemExit(f"Missing source contract: {label}")


def main() -> int:
    vba = (ROOT / "src/Shell/VBA/example1.bas").read_text(encoding="utf-8")
    for pattern, label in [
        (r"^Option Explicit$", "VBA Option Explicit"),
        (r"Private\s+Enum\s+ThemeFamily.*DarkTheme.*LightTheme.*End\s+Enum", "VBA family enum"),
        (r"Private\s+Type\s+UIFactory.*Family\s+As\s+ThemeFamily.*End\s+Type", "VBA UIFactory value"),
        (r"Function\s+CreateButton\s*\(ByRef\s+factory\s+As\s+UIFactory\).*Select\s+Case\s+factory\.Family.*DarkTheme.*Dark Button.*LightTheme.*Light Button", "VBA button family mapping"),
        (r"Function\s+CreateCheckbox\s*\(ByRef\s+factory\s+As\s+UIFactory\).*Select\s+Case\s+factory\.Family.*DarkTheme.*Dark Checkbox.*LightTheme.*Light Checkbox", "VBA checkbox family mapping"),
        (r"factory\s*=\s*CreateFactory\(DarkTheme\).*CreateButton\(factory\).*CreateCheckbox\(factory\).*factory\s*=\s*CreateFactory\(LightTheme\).*CreateButton\(factory\).*CreateCheckbox\(factory\)", "VBA one selected factory per family"),
    ]:
        require(vba, pattern, label)

    vba_memento = (ROOT / "src/Shell/VBA/memento.bas").read_text(encoding="utf-8")
    for pattern, label in [
        (r"^Option Explicit$", "VBA Memento Option Explicit"),
        (r"Private\s+Type\s+MementoSnapshot.*Title\s+As\s+String.*Tags\s+As\s+String.*End\s+Type", "VBA Memento snapshot value"),
        (r"Private\s+Type\s+Document.*Title\s+As\s+String.*Tags\s+As\s+String.*End\s+Type", "VBA Memento originator value"),
        (r"Function\s+SaveMemento\s*\(ByRef\s+originator\s+As\s+Document\)\s+As\s+MementoSnapshot.*snapshot\.Title\s*=\s*originator\.Title.*snapshot\.Tags\s*=\s*originator\.Tags", "VBA originator owns capture"),
        (r"Sub\s+RestoreMemento\s*\(ByRef\s+originator\s+As\s+Document,\s*ByRef\s+snapshot\s+As\s+MementoSnapshot\).*originator\.Title\s*=\s*snapshot\.Title.*originator\.Tags\s*=\s*snapshot\.Tags", "VBA originator owns restore"),
        (r"caretakerSnapshot\s*=\s*SaveMemento\(originator\).*originator\.Title\s*=\s*\"published\".*RestoreMemento\s+originator,\s*caretakerSnapshot.*Debug\.Assert\s+originator\.Title\s*=\s*\"draft\".*Debug\.Assert\s+caretakerSnapshot\.Title\s*=\s*\"draft\"", "VBA Memento mutation restore and snapshot independence"),
    ]:
        require(vba_memento, pattern, label)

    delphi = (ROOT / "src/Enterprise/Delphi/Example1.pas").read_text(encoding="utf-8")
    for pattern, label in [
        (r"IUIFactory\s*=\s*interface.*function\s+CreateButton:\s*IButton;.*function\s+CreateCheckbox:\s*ICheckbox;", "Delphi abstract factory interface"),
        (r"TDarkFactory\s*=\s*class\(TInterfacedObject,\s*IUIFactory\)", "Delphi dark factory"),
        (r"TLightFactory\s*=\s*class\(TInterfacedObject,\s*IUIFactory\)", "Delphi light factory"),
        (r"function\s+TDarkFactory\.CreateButton:\s*IButton;.*Result\s*:=\s*TDarkButton\.Create", "Delphi dark button mapping"),
        (r"function\s+TDarkFactory\.CreateCheckbox:\s*ICheckbox;.*Result\s*:=\s*TDarkCheckbox\.Create", "Delphi dark checkbox mapping"),
        (r"function\s+TLightFactory\.CreateButton:\s*IButton;.*Result\s*:=\s*TLightButton\.Create", "Delphi light button mapping"),
        (r"function\s+TLightFactory\.CreateCheckbox:\s*ICheckbox;.*Result\s*:=\s*TLightCheckbox\.Create", "Delphi light checkbox mapping"),
        (r"procedure\s+CreateUIComponents\(factory:\s*IUIFactory\).*factory\.CreateButton.*factory\.CreateCheckbox", "Delphi client consumes one factory"),
        (r"CreateUIComponents\(TDarkFactory\.Create\);.*CreateUIComponents\(TLightFactory\.Create\);", "Delphi both coherent families"),
    ]:
        require(delphi, pattern, label)

    delphi_memento = (ROOT / "src/Enterprise/Delphi/Memento.pas").read_text(encoding="utf-8")
    for pattern, label in [
        (r"TMementoSnapshot\s*=\s*record.*Title:\s*string;.*Tags:\s*string;.*end;", "Delphi Memento snapshot record"),
        (r"TDocument\s*=\s*class.*function\s+SaveMemento:\s*TMementoSnapshot;.*procedure\s+RestoreMemento\(const\s+Snapshot:\s*TMementoSnapshot\);", "Delphi originator capture/restore API"),
        (r"function\s+TDocument\.SaveMemento:\s*TMementoSnapshot;.*Result\.Title\s*:=\s*FTitle;.*Result\.Tags\s*:=\s*FTags;", "Delphi originator owns capture"),
        (r"procedure\s+TDocument\.RestoreMemento\(const\s+Snapshot:\s*TMementoSnapshot\);.*FTitle\s*:=\s*Snapshot\.Title;.*FTags\s*:=\s*Snapshot\.Tags;", "Delphi originator owns restore"),
        (r"CaretakerSnapshot\s*:=\s*Originator\.SaveMemento;.*Originator\.Title\s*:=\s*'published';.*Originator\.RestoreMemento\(CaretakerSnapshot\);.*Originator\.Title\s*<>\s*'draft'.*CaretakerSnapshot\.Title\s*<>\s*'draft'", "Delphi Memento mutation restore and snapshot independence"),
    ]:
        require(delphi_memento, pattern, label)

    print("VBA Abstract Factory source contract: OK")
    print("VBA Memento source contract: OK")
    print("Delphi Abstract Factory source contract: OK")
    print("Delphi Memento source contract: OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
