adapter_dir = fileparts(mfilename('fullpath'));
repo = fileparts(fileparts(fileparts(adapter_dir)));
addpath(fullfile(repo, 'src', 'DataScience', 'MATLAB'));

contracts = {
    'example1', {'Dark Button', 'Dark Checkbox', 'Light Button', 'Light Checkbox'};
    'adapter', {'legacy=86F', 'adapted=30C'};
    'bridge', {'basic-tv=TV:on', 'basic-radio=Radio:on', 'mute-tv=TV:muted', 'mute-radio=Radio:muted'};
    'builder', {'# Service status', '99.95%', '<h1>Service status</h1>'};
    'chain_of_responsibility', {'visited=faq>billing;handled=billing;result=refund(250)'};
    'composite', {'leaf=2', 'docs=8', 'root=10'};
    'decorator', {'base=alert', 'audit=audit(alert)', 'encrypted=enc(alert)', 'stacked=audit(enc(alert))'};
    'facade', {'checkout=auth(alice)>reserve(SKU-42)>charge(499)'};
    'factory_method', {'PostgreSQL connect', 'PostgreSQL query', 'MySQL connect', 'MySQL query'};
    'flyweight', {'styles=2;shared=true;text=ABC'};
    'prototype', {'original=orders: metrics', 'clone=orders-canary: metrics,tracing'};
    'proxy', {'backend=1;fetches=1;first=doc(42);second=doc(42)'};
    'singleton', {'same=true', 'count=1'};
};

for i = 1:size(contracts, 1)
    name = contracts{i, 1};
    markers = contracts{i, 2};
    output = evalc(name);
    disp(output);
    for j = 1:numel(markers)
        assert(contains(output, markers{j}), sprintf('MATLAB %s missing marker: %s', name, markers{j}));
    end
    if strcmp(name, 'prototype')
        assert(~contains(output, 'original=orders: metrics,tracing'), 'MATLAB Prototype shares mutable feature state');
    end
end

validate_pattern_sweep;
disp('MATLAB clean-slate contracts: OK');
