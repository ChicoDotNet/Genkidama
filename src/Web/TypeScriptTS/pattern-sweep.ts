type Check = [string, () => boolean];

function commandPattern() { const q=[(x:number)=>x+50,(x:number)=>x-20]; const balance=q.reduce((v,f)=>f(v),100); return balance===130 && q[1](150)===130; }
type Expr={kind:'lit',v:number}|{kind:'add'|'mul',l:Expr,r:Expr};
function evalExpr(e:Expr):number{return e.kind==='lit'?e.v:e.kind==='add'?evalExpr(e.l)+evalExpr(e.r):evalExpr(e.l)*evalExpr(e.r)}
function interpreterPattern(){return evalExpr({kind:'add',l:{kind:'lit',v:7},r:{kind:'mul',l:{kind:'lit',v:3},r:{kind:'lit',v:4}}})===19}
function iteratorPattern(){const it=[10,20,30][Symbol.iterator](); const seen=[it.next().value,it.next().value,it.next().value]; return seen.join(',')==='10,20,30' && it.next().done===true}
function mediatorPattern(){const events:string[]=[]; const notify=(s:string,e:string)=>{if(s==='button'&&e==='click')events.push('panel.refresh');if(s==='panel'&&e==='loaded')events.push('button.enable')};notify('button','click');notify('panel','loaded');return events.join('>')==='panel.refresh>button.enable'}
function mementoPattern(){let state='draft';const snapshot=state;state='published';state=snapshot;return state==='draft'}
function observerPattern(){const observers=[(id:number)=>`audit:${id}`,(id:number)=>`dashboard:${id}`];return observers.map(o=>o(42)).join('>')==='audit:42>dashboard:42'}
function statePattern(){const transition=(s:string,a:string)=>s==='locked'&&a==='unlock'?'unlocked':s==='unlocked'&&a==='lock'?'locked':s;return transition(transition('locked','unlock'),'lock')==='locked'}
function strategyPattern(){const price=(v:number,s:(x:number)=>number)=>s(v);return price(100,x=>x)===100&&price(100,x=>x*0.8)===80}
function templateMethodPattern(){const pipeline=(read:string,transform:()=>string)=>`${read}>${transform()}>publish`;return pipeline('read-csv',()=> 'normalize')==='read-csv>normalize>publish'}
type Shape={kind:'circle',r:number}|{kind:'rect',w:number,h:number};function area(s:Shape){return s.kind==='circle'?Math.PI*s.r*s.r:s.w*s.h}function visitorPattern(){return Math.abs([{kind:'circle',r:2},{kind:'rect',w:3,h:4}] .map(x=>area(x as Shape)).reduce((a,b)=>a+b,0)-(4*Math.PI+12))<1e-9}
function mvcPattern(){let count=0;const view=()=>`count=${count}`;const before=view();count++;return before==='count=0'&&view()==='count=1'}
function mvvmPattern(){let amount=10;const text=()=>`$${amount}.00`;const before=text();amount+=5;return before==='$10.00'&&text()==='$15.00'}
function microkernelPattern(){const plugins={double:(x:number)=>x*2,square:(x:number)=>x*x};return plugins.double(4)===8&&plugins.square(4)===16}
function microservicesPattern(){let stock=7;const reserve=(q:number)=>q>stock?false:(stock-=q,true);const place=(q:number)=>reserve(q)?'confirmed':'rejected';return place(2)==='confirmed'&&stock===5}
function enterpriseAdapterPattern(){const legacy={code:17,cents:1250};const canonical={id:legacy.code,amount:legacy.cents/100};return canonical.id===17&&canonical.amount===12.5}
function enterpriseBridgePattern(){const send=(transport:string,kind:string,msg:string)=>`${transport}>${kind}:${msg}`;return send('kafka','ALERT','disk')==='kafka>ALERT:disk'&&send('queue','REMINDER','backup')==='queue>REMINDER:backup'}
function enterpriseFacadePattern(){const crm=(id:number)=>`crm:create:${id}`,billing=(id:number)=>`billing:open:${id}`;return `${crm(77)}>${billing(77)}`==='crm:create:77>billing:open:77'}
function brokerPattern(){const services={inventory:(k:string)=>`inventory:${k}=7`,customer:(k:string)=>`customer:${k}=active`};return services.inventory('sku-1')==='inventory:sku-1=7'&&services.customer('17')==='customer:17=active'}
function messageBusPattern(){const handlers=[(t:string,id:number)=>`audit:${t}:${id}`,(t:string,id:number)=>`billing:${t}:${id}`];return handlers.map(h=>h('order-created',42)).join('>')==='audit:order-created:42>billing:order-created:42'}
function serviceLocatorPattern(){const services={email:(v:string)=>`email>${v}`,audit:(v:string)=>`audit>${v}`};return services.email('a@example.test')==='email>a@example.test'&&services.audit('created')==='audit>created'}
function activeObjectPattern(){let value=0;const queue=[()=>value+=3,()=>value*=4];const before=value;queue.forEach(f=>f());return before===0&&value===12}
function monitorObjectPattern(){class Counter{#value=0;add(x:number){this.#value+=x}get value(){return this.#value}}const c=new Counter();c.add(2);c.add(3);return c.value===5}
function halfSyncHalfAsyncPattern(){const asyncIngress=['job-1','job-2','job-3'];const syncCore=asyncIngress.map(j=>`done:${j}`);return syncCore.join('>')==='done:job-1>done:job-2>done:job-3'}
function leaderFollowersPattern(){const workers=['worker-1','worker-2','worker-3'],events=['a','b','c'];const handled=events.map((e,i)=>`${workers[i%workers.length]}:${e}`);return handled.join('>')==='worker-1:a>worker-2:b>worker-3:c'&&workers[events.length%workers.length]==='worker-1'}
function clientServerPattern(){const server=(k:string)=>k==='sku-1'?{status:200,body:'stock=7'}:{status:404,body:'missing'};const r=server('sku-1');return r.status===200&&r.body==='stock=7'}
function peerToPeerPattern(){const inbox:string[]=[];const send=(from:string,to:string,data:string)=>inbox.push(`${from}>${to}:${data}`);send('peer-a','peer-b','block-42');send('peer-a','peer-c','block-42');return inbox.join('>')==='peer-a>peer-b:block-42>peer-a>peer-c:block-42'}
function publishSubscribePattern(){const subs=[(id:number)=>`warehouse:${id}`,(id:number)=>`analytics:${id}`];return subs.map(s=>s(51)).join('>')==='warehouse:51>analytics:51'}
function distributedProxyPattern(){const remote=(sku:string)=>sku==='sku-1'?7:0;const proxy=(sku:string)=>remote(sku);return proxy('sku-1')===7}
function pacPattern(){const view=(name:string,value:number)=>`${name}:view=${value}`;return view('child',42)==='child:view=42'&&view('root',42)==='root:view=42'}
function mvpPattern(){let count=0,text='';const present=()=>{count++;text=`count=${count}`};present();return count===1&&text==='count=1'}
function documentViewPattern(){const doc={title:'Final',words:120};return `editor:${doc.title}:${doc.words}`==='editor:Final:120'&&`summary:${doc.title}`==='summary:Final'}
function activeRecordPattern(){const table=new Map<number,string>();table.set(7,'Ada');return table.get(7)==='Ada'}
function dataMapperPattern(){const person={id:8,name:'Grace'};const row={key:`person:${person.id}`,name:person.name};return row.key==='person:8'&&row.name==='Grace'}
function unitOfWorkPattern(){const store:number[]=[],pending=[2,3];store.push(...pending);pending.length=0;return store.join(',')==='2,3'&&pending.length===0}
function repositoryPattern(){const rows=[{id:1,name:'Ada'},{id:2,name:'Grace'}];return rows.find(x=>x.id===2)?.name==='Grace'}
function dependencyInjectionPattern(){const service=(clock:()=>string)=>`at:${clock()}`;return service(()=> '10:00')==='at:10:00'}
function lazyInitializationPattern(){let builds=0,cache:string|undefined;const get=()=>cache??=(builds++,'ready');return get()==='ready'&&get()==='ready'&&builds===1}
function objectPoolPattern(){const pool=[1,2];const x=pool.pop()!;pool.push(x);return pool.length===2&&pool.includes(x)}
function nullObjectPattern(){interface Logger{log(m:string):string}const nil:Logger={log:()=>''},real:Logger={log:m=>`log:${m}`};return nil.log('x')===''&&real.log('x')==='log:x'}

const cases:Check[]=[
['Command',commandPattern],['Interpreter',interpreterPattern],['Iterator',iteratorPattern],['Mediator',mediatorPattern],['Memento',mementoPattern],['Observer',observerPattern],['State',statePattern],['Strategy',strategyPattern],['Template Method',templateMethodPattern],['Visitor',visitorPattern],['MVC',mvcPattern],['MVVM',mvvmPattern],['Microkernel',microkernelPattern],['Microservices',microservicesPattern],['Enterprise Adapter',enterpriseAdapterPattern],['Enterprise Bridge',enterpriseBridgePattern],['Enterprise Facade',enterpriseFacadePattern],['Broker',brokerPattern],['Message Bus',messageBusPattern],['Service Locator',serviceLocatorPattern],['Active Object',activeObjectPattern],['Monitor Object',monitorObjectPattern],['Half-Sync / Half-Async',halfSyncHalfAsyncPattern],['Leader / Followers',leaderFollowersPattern],['Client-Server',clientServerPattern],['Peer-to-Peer',peerToPeerPattern],['Publish-Subscribe',publishSubscribePattern],['Distributed Proxy',distributedProxyPattern],['Presentation-Abstraction-Control',pacPattern],['Model-View-Presenter',mvpPattern],['Document-View',documentViewPattern],['Active Record',activeRecordPattern],['Data Mapper',dataMapperPattern],['Unit of Work',unitOfWorkPattern],['Repository',repositoryPattern],['Dependency Injection',dependencyInjectionPattern],['Lazy Initialization',lazyInitializationPattern],['Object Pool',objectPoolPattern],['Null Object',nullObjectPattern]];
for(const [name,check] of cases){if(!check())throw new Error(`pattern failed: ${name}`)}
if(cases.length!==39)throw new Error(`expected 39 cases, got ${cases.length}`);
console.log('TypeScript pattern sweep: 39/39 examples passed');
