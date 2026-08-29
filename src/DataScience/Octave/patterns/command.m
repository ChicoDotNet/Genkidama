function command(); b=100; ops={@(x)x+50,@(x)x-20}; for i=1:numel(ops); b=ops{i}(b); end; assert(b==130); end
