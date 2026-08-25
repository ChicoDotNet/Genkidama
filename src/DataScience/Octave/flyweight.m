function flyweight()
  pool = containers.Map();

  [pool, red1] = get_style(pool, 'Inter', 12, 'red');
  [pool, red2] = get_style(pool, 'Inter', 12, 'red');
  [pool, blue] = get_style(pool, 'Inter', 12, 'blue');

  assert(strcmp(blue.color, 'blue'));
  shared = strcmp(red1.font, red2.font) && red1.size == red2.size && strcmp(red1.color, red2.color);
  fprintf('styles=%d;shared=%s;text=ABC\n', pool.Count, lower(mat2str(shared)));
end

function [pool, style] = get_style(pool, font, size, color)
  key = sprintf('%s|%d|%s', font, size, color);
  if ~isKey(pool, key)
    pool(key) = struct('font', font, 'size', size, 'color', color);
  end
  style = pool(key);
end

flyweight();
