proc run*(): bool =
  proc crm(id: int): string = "crm:create:" & $id
  proc billing(id: int): string = "billing:open:" & $id
  crm(77) & ">" & billing(77) == "crm:create:77>billing:open:77"
