module DataMapperExample

type Person = { Id: int; Name: string }
type Row = { Key: string; Name: string }

let run () =
    let person = { Id = 8; Name = "Grace" }
    let row = { Key = $"person:{person.Id}"; Name = person.Name }
    let restored = { Id = int (row.Key.Split(':')[1]); Name = row.Name }
    row.Key = "person:8" && restored = person
