function result = document_view
%DOCUMENT_VIEW Render the same document through independent views.
document = struct("title", "Draft", "words", 120);
document.title = "Final";
editor = editorView(document);
summary = summaryView(document);
result = struct("editor", editor, "summary", summary);
end

function text = editorView(document)
text = "editor:" + document.title + ":" + string(document.words);
end

function text = summaryView(document)
text = "summary:" + document.title;
end
