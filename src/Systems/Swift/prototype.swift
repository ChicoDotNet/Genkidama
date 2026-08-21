struct ServiceProfile {
    var name: String
    var features: [String]

    func clone() -> ServiceProfile {
        self
    }

    func describe() -> String {
        "\(name): \(features.joined(separator: ","))"
    }
}

let original = ServiceProfile(name: "orders", features: ["metrics"])
var canary = original.clone()

canary.name = "orders-canary"
canary.features.append("tracing")

print("original=\(original.describe())")
print("clone=\(canary.describe())")
