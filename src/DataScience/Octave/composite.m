function composite()
  readme = file_node(2);
  docs = folder_node({file_node(3), file_node(5)});
  root = folder_node({readme, docs});

  fprintf('leaf=%d\n', node_size(readme));
  fprintf('docs=%d\n', node_size(docs));
  fprintf('root=%d\n', node_size(root));
end

function node = file_node(bytes)
  node = struct('kind', 'file', 'bytes', bytes, 'children', {{}});
end

function node = folder_node(children)
  node = struct('kind', 'folder', 'bytes', 0, 'children', {children});
end

function total = node_size(node)
  if strcmp(node.kind, 'file')
    total = node.bytes;
    return;
  end

  total = 0;
  for index = 1:numel(node.children)
    total = total + node_size(node.children{index});
  end
end
