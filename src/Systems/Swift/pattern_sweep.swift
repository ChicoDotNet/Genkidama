import Foundation

let fileManager = FileManager.default
let repositoryRoot = URL(fileURLWithPath: fileManager.currentDirectoryPath, isDirectory: true)
let patternsDirectory = repositoryRoot
    .appendingPathComponent("src", isDirectory: true)
    .appendingPathComponent("Systems", isDirectory: true)
    .appendingPathComponent("Swift", isDirectory: true)
    .appendingPathComponent("patterns", isDirectory: true)

let patternFiles = try fileManager
    .contentsOfDirectory(at: patternsDirectory, includingPropertiesForKeys: nil)
    .filter { $0.pathExtension == "swift" }
    .sorted { $0.lastPathComponent < $1.lastPathComponent }

precondition(patternFiles.count == 39, "expected 39 canonical Swift examples, got \(patternFiles.count)")

let workDirectory = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
    .appendingPathComponent("genkidama-swift-sweep-\(UUID().uuidString)", isDirectory: true)
try fileManager.createDirectory(at: workDirectory, withIntermediateDirectories: true)
defer { try? fileManager.removeItem(at: workDirectory) }

let driver = workDirectory.appendingPathComponent("main.swift")
var driverLines = ["let cases: [(String, () -> Bool)] = ["]
for file in patternFiles {
    let base = file.deletingPathExtension().lastPathComponent
    driverLines.append("    (\"\(base)\", \(base)Example.run),")
}
driverLines.append("]")
driverLines.append("for (name, run) in cases { precondition(run(), \"pattern failed: \\(name)\") }")
driverLines.append("precondition(cases.count == 39, \"expected 39 cases\")")
driverLines.append("print(\"Swift pattern sweep: 39/39 examples passed\")")
try (driverLines.joined(separator: "\n") + "\n").write(to: driver, atomically: true, encoding: .utf8)

let binary = workDirectory.appendingPathComponent("swift-pattern-sweep")
let compiler = Process()
compiler.executableURL = URL(fileURLWithPath: "/usr/bin/env")
compiler.arguments = ["swiftc", "-warnings-as-errors"] + patternFiles.map(\.path) + [driver.path, "-o", binary.path]
compiler.standardOutput = FileHandle.standardOutput
compiler.standardError = FileHandle.standardError
try compiler.run()
compiler.waitUntilExit()
precondition(compiler.terminationStatus == 0, "canonical Swift examples failed to compile")

let executable = Process()
executable.executableURL = binary
executable.standardOutput = FileHandle.standardOutput
executable.standardError = FileHandle.standardError
try executable.run()
executable.waitUntilExit()
precondition(executable.terminationStatus == 0, "canonical Swift examples failed at runtime")
