function nullObjectPattern(){interface Logger{log(m:string):string}const nil:Logger={log:()=>''},real:Logger={log:m=>`log:${m}`};return nil.log('x')===''&&real.log('x')==='log:x'}
