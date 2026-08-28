proc run*(): bool =
  proc send(transport, kind, message: string): string = transport & ">" & kind & ":" & message
  send("kafka", "ALERT", "disk") == "kafka>ALERT:disk" and send("queue", "REMINDER", "backup") == "queue>REMINDER:backup"
