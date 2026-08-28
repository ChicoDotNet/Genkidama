import std/strutils
proc run*(): bool =
  var events: seq[string] = @[]
  proc notify(sender, event: string) =
    if sender == "button" and event == "click": events.add("panel.refresh")
    elif sender == "panel" and event == "loaded": events.add("button.enable")
  notify("button", "click"); notify("panel", "loaded")
  events.join(">") == "panel.refresh>button.enable"
