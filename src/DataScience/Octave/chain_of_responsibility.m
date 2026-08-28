function chain_of_responsibility()
  handlers = {
    struct('name', 'faq', 'limit', 50),
    struct('name', 'billing', 'limit', 500),
    struct('name', 'escalation', 'limit', Inf)
  };

  amount = 250;
  visited = {};
  handled = '';

  for i = 1:numel(handlers)
    handler = handlers{i};
    visited{end + 1} = handler.name;
    if amount <= handler.limit
      handled = handler.name;
      break;
    end
  end

  if isempty(handled)
    error('No handler accepted the request');
  end

  fprintf('visited=%s;handled=%s;result=refund(%d)\n', ...
    strjoin(visited, '>'), handled, amount);
end
