import std/[sequtils, strutils]
proc run*(): bool =
  let asyncIngress = @["job-1", "job-2", "job-3"]
  let syncCore = asyncIngress.mapIt("done:" & it)
  syncCore.join(">") == "done:job-1>done:job-2>done:job-3"
