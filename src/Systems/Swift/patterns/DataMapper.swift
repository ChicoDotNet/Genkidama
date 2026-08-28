enum DataMapperExample {
    static func run() -> Bool {
        let person = (id: 8, name: "Grace")
        let row = (key: "person:\(person.id)", name: person.name)
        let restored = (id: Int(row.key.split(separator: ":")[1])!, name: row.name)
        return row.key == "person:8" && restored.id == person.id && restored.name == person.name
    }
}
