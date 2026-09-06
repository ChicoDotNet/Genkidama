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

    delphi_state = (ROOT / "src/Enterprise/Delphi/State.pas").read_text(encoding="utf-8")
    for pattern, label in [
        (r"TGateState\s*=\s*\(\s*gsLocked\s*,\s*gsUnlocked\s*,\s*gsInvalid\s*\)", "Delphi State enum"),
        (r"function\s+Transition\s*\(\s*CurrentState:\s*TGateState;\s*const\s+Action:\s*string\s*\):\s*TGateState", "Delphi State transition function"),
        (r"case\s+CurrentState\s+of.*gsLocked:.*Action\s*=\s*'coin'.*Result\s*:=\s*gsUnlocked.*Result\s*:=\s*gsLocked", "Delphi locked transition contract"),
        (r"case\s+CurrentState\s+of.*gsUnlocked:.*Action\s*=\s*'push'.*Result\s*:=\s*gsLocked.*Result\s*:=\s*gsUnlocked", "Delphi unlocked transition contract"),
        (r"else\s+Result\s*:=\s*gsInvalid", "Delphi unknown-state rejection"),
        (r"State\s*:=\s*gsLocked;.*RequireState\(State,\s*gsLocked,\s*'initial state must be locked'\)", "Delphi initial state assertion"),
        (r"State\s*:=\s*Transition\(State,\s*'push'\);\s*RequireState\(State,\s*gsLocked,\s*'push while locked must preserve state'\)", "Delphi invalid push assertion"),
        (r"State\s*:=\s*Transition\(State,\s*'coin'\);\s*RequireState\(State,\s*gsUnlocked,\s*'coin while locked must unlock'\)", "Delphi unlock assertion"),
        (r"State\s*:=\s*Transition\(State,\s*'coin'\);\s*RequireState\(State,\s*gsUnlocked,\s*'duplicate coin must preserve unlocked state'\)", "Delphi duplicate coin assertion"),
        (r"State\s*:=\s*Transition\(State,\s*'push'\);\s*RequireState\(State,\s*gsLocked,\s*'push while unlocked must lock'\)", "Delphi relock assertion"),
        (r"State\s*:=\s*Transition\(gsInvalid,\s*'coin'\);\s*RequireState\(State,\s*gsInvalid,\s*'unknown state must remain invalid'\)", "Delphi invalid-state assertion"),
        (r"Writeln\('delphi-state:\s*passed'\)", "Delphi State sentinel"),
    ]:
        require(delphi_state, pattern, label)

    print("VBA Abstract Factory source contract: OK")
    print("Delphi Abstract Factory source contract: OK")
    print("Delphi State source contract: OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
