proc run*(): bool =
  let legacyCode = 17
  let legacyCents = 1250
  let canonicalId = legacyCode
  let canonicalAmount = legacyCents.float / 100.0
  canonicalId == 17 and canonicalAmount == 12.5
