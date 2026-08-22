def clone_profile(profile):
    return {
        "name": profile["name"],
        "features": list(profile["features"]),
    }


def describe(profile):
    return "{}: {}".format(profile["name"], ",".join(profile["features"]))


original = {"name": "orders", "features": ["metrics"]}
canary = clone_profile(original)
canary["name"] = "orders-canary"
canary["features"].append("tracing")

print("original=" + describe(original))
print("clone=" + describe(canary))
