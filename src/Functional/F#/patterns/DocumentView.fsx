module DocumentViewExample

type Document = { Title: string; Words: int }

let run () =
    let document = { Title = "Final"; Words = 120 }
    let editorView (doc: Document) = $"editor:{doc.Title}:{doc.Words}"
    let summaryView (doc: Document) = $"summary:{doc.Title}"
    editorView document = "editor:Final:120"
    && summaryView document = "summary:Final"
