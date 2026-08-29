def received=[];def topics=[news:[{v->received<<v}]];topics.news*.call('v1');assert received==['v1']
