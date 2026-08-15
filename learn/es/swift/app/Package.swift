// swift-tools-version: 6.3

import PackageDescription

let package = Package(
    name: "TimeQuote",
    products: [
        .executable(name: "TimeQuote", targets: ["TimeQuote"]),
    ],
    targets: [
        .executableTarget(name: "TimeQuote"),
        .testTarget(name: "TimeQuoteTests", dependencies: ["TimeQuote"]),
    ]
)
