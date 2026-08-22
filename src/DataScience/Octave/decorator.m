function decorator()
  base = @base_render;
  audited = audit_decorator(base);
  encrypted = encrypt_decorator(base);
  stacked = audit_decorator(encrypt_decorator(base));

  fprintf('base=%s\n', base());
  fprintf('audit=%s\n', audited());
  fprintf('encrypted=%s\n', encrypted());
  fprintf('stacked=%s\n', stacked());
end

function output = base_render()
  output = 'alert';
end

function wrapped = audit_decorator(component)
  wrapped = @render;
  function output = render()
    output = ['audit(' component() ')'];
  end
end

function wrapped = encrypt_decorator(component)
  wrapped = @render;
  function output = render()
    output = ['enc(' component() ')'];
  end
end
