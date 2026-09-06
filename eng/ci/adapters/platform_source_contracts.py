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

    vba_mediator = (ROOT / "src/Shell/VBA/MediatorExample.bas").read_text(encoding="utf-8")
    for pattern, label in [
        (r"^Option Explicit$", "VBA Mediator Option Explicit"),
        (r"Function\s+RouteMessage\s*\(.*senderName.*targetName.*messageText.*Select\s+Case\s+targetName", "VBA Mediator owns routing"),
        (r"PaymentSend.*RouteMessage\(\"payment\",\s*\"inventory\"", "VBA payment routes through mediator"),
        (r"InventorySend.*RouteMessage\(\"inventory\",\s*\"payment\"", "VBA inventory routes through mediator"),
        (r"UnknownColleague:.*targetName", "VBA Mediator unknown colleague failure"),
        (r"VBA Mediator: passed", "VBA Mediator verification sentinel"),
    ]:
        require(vba_mediator, pattern, label)

    delphi_mediator = (ROOT / "src/Enterprise/Delphi/MediatorExample.pas").read_text(encoding="utf-8")
    for pattern, label in [
        (r"TCheckoutMediator\s*=\s*class.*function\s+Route", "Delphi Mediator owns routing"),
        (r"TPaymentColleague.*FMediator:\s*TCheckoutMediator.*function\s+Send", "Delphi payment colleague depends on mediator"),
        (r"TInventoryColleague.*FMediator:\s*TCheckoutMediator.*function\s+Send", "Delphi inventory colleague depends on mediator"),
        (r"Route\('payment',\s*'inventory'", "Delphi payment routes through mediator"),
        (r"Route\('inventory',\s*'payment'", "Delphi inventory routes through mediator"),
        (r"UnknownColleague:.*TargetName", "Delphi Mediator unknown colleague failure"),
        (r"Delphi Mediator: passed", "Delphi Mediator verification sentinel"),
    ]:
        require(delphi_mediator, pattern, label)

    print("VBA Abstract Factory source contract: OK")
    print("Delphi Abstract Factory source contract: OK")
    print("VBA Mediator source contract: OK")
    print("Delphi Mediator source contract: OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())