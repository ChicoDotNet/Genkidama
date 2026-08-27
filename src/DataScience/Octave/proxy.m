function proxy()
  %PROXY Demonstrate a lazy caching Proxy with one mediated backend fetch.
  %   PROXY() creates the real subject on first access, caches document 42,
  %   performs the same lookup twice, and prints the canonical deterministic
  %   observable used by the cross-language Proxy validation gate.
  backend_created = 0;
  fetches = 0;
  cache = containers.Map('KeyType', 'char', 'ValueType', 'char');
  subject_created = false;

  first = get_document(42);
  second = get_document(42);
  printf('backend=%d;fetches=%d;first=%s;second=%s\n', backend_created, fetches, first, second);

  function value = get_document(id)
    key = num2str(id);
    if isKey(cache, key)
      value = cache(key);
      return;
    endif

    if !subject_created
      backend_created = backend_created + 1;
      subject_created = true;
    endif

    fetches = fetches + 1;
    value = sprintf('doc(%s)', key);
    cache(key) = value;
  endfunction
endfunction
