function lazy_initialization(); cache=[]; if isempty(cache); cache=7; end; first=cache; if isempty(cache); cache=8; end; assert(first==cache); end
