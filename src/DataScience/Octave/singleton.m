function singleton()
  first = registry_instance();
  second = registry_instance();
  registry_increment();

  fprintf('same=%s\n', bool_text(first == second));
  fprintf('count=%d\n', registry_count());
end

function token = registry_instance()
  token = 1;
end

function registry_increment()
  registry_state('increment');
end

function count = registry_count()
  count = registry_state('read');
end

function value = registry_state(action)
  persistent count = 0;

  if strcmp(action, 'increment')
    count = count + 1;
  end

  value = count;
end

function value = bool_text(flag)
  if flag
    value = 'true';
  else
    value = 'false';
  end
end
