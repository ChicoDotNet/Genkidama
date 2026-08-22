import std/[sequtils, strutils]

type
  ServiceProfile = object
    name: string
    features: seq[string]

proc cloneProfile(profile: ServiceProfile): ServiceProfile =
  ServiceProfile(name: profile.name, features: profile.features.mapIt(it))

proc describe(profile: ServiceProfile): string =
  profile.name & ": " & profile.features.join(",")

var original = ServiceProfile(name: "orders", features: @["metrics"])
var canary = cloneProfile(original)
canary.name = "orders-canary"
canary.features.add("tracing")

echo "original=" & describe(original)
echo "clone=" & describe(canary)
