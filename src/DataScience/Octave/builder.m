function builder()
  disp(build_availability_report(text_builder()));
  disp('---');
  disp(build_availability_report(html_builder()));
end

function b = text_builder()
  parts = {};
  b.reset = @reset;
  b.add_title = @add_title;
  b.add_section = @add_section;
  b.build = @build;

  function reset()
    parts = {};
  end
  function add_title(title)
    parts{end + 1} = ['# ' title];
  end
  function add_section(heading, body)
    parts{end + 1} = ['## ' heading];
    parts{end + 1} = body;
  end
  function result = build()
    result = strjoin(parts, sprintf('\n'));
  end
end

function b = html_builder()
  parts = {};
  b.reset = @reset;
  b.add_title = @add_title;
  b.add_section = @add_section;
  b.build = @build;

  function reset()
    parts = {};
  end
  function add_title(title)
    parts{end + 1} = ['<h1>' title '</h1>'];
  end
  function add_section(heading, body)
    parts{end + 1} = ['<h2>' heading '</h2>'];
    parts{end + 1} = ['<p>' body '</p>'];
  end
  function result = build()
    result = strjoin(parts, '');
  end
end

function result = build_availability_report(b)
  b.reset();
  b.add_title('Service status');
  b.add_section('Availability', '99.95%');
  result = b.build();
end
