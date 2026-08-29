def seen=[];def subs=[{e->seen<<e}];subs*.call('changed');assert seen==['changed']
