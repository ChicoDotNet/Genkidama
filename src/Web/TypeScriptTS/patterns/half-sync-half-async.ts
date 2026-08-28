function halfSyncHalfAsyncPattern(){const asyncIngress=['job-1','job-2','job-3'];const syncCore=asyncIngress.map(j=>`done:${j}`);return syncCore.join('>')==='done:job-1>done:job-2>done:job-3'}
