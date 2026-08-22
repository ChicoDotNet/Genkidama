function composite
    readme = fileNode(2);
    docs = folderNode({fileNode(3), fileNode(5)});
    root = folderNode({readme, docs});

    fprintf('leaf=%d\n', nodeSize(readme));
    fprintf('docs=%d\n', nodeSize(docs));
    fprintf('root=%d\n', nodeSize(root));
end

function node = fileNode(bytes)
    node = struct('kind', "file", 'bytes', bytes, 'children', {{}});
end

function node = folderNode(children)
    node = struct('kind', "folder", 'bytes', 0, 'children', {children});
end

function total = nodeSize(node)
    if node.kind == "file"
        total = node.bytes;
        return;
    end

    total = 0;
    for index = 1:numel(node.children)
        total = total + nodeSize(node.children{index});
    end
end
