package main

import (
	"fmt"
	"strings"
)

type expr interface{ eval() int }
type lit int
func (n lit) eval() int { return int(n) }
type add struct{ a, b expr }
func (e add) eval() int { return e.a.eval() + e.b.eval() }
type mul struct{ a, b expr }
func (e mul) eval() int { return e.a.eval() * e.b.eval() }

type shape interface{ area() int; perimeter() int }
type circle struct{ r int }
func (c circle) area() int { return 3*c.r*c.r }
func (c circle) perimeter() int { return 6*c.r }
type rectangle struct{ w, h int }
func (r rectangle) area() int { return r.w*r.h }
func (r rectangle) perimeter() int { return 2*(r.w+r.h) }

type document struct{ text string }
type record struct{ id int; name string }
type testCase struct{ name string; ok bool }

func commandExample() bool {
	commands := []int{10, -3}
	balance := 0
	for _, c := range commands { balance += c }
	undone := balance - commands[len(commands)-1]
	return balance == 7 && undone == 10
}
func interpreterExample() bool { return (add{lit(2), mul{lit(3), lit(4)}}).eval() == 14 }
func iteratorExample() bool {
	next := func(xs []int, cursor int) (int, int, bool) {
		if cursor >= len(xs) { return 0, cursor, false }
		return xs[cursor], cursor+1, true
	}
	v, c, ok := next([]int{10,20}, 0); _, _, end := next([]int{10,20}, 2)
	return ok && v == 10 && c == 1 && !end
}
func mediatorExample() bool {
	mediate := func(sender, msg string) (string,string) { if sender == "sales" { return "billing", msg }; return "sales", msg }
	to, msg := mediate("sales", "invoice"); return to == "billing" && msg == "invoice"
}
func mementoExample() bool { current := document{"v2"}; snapshot := document{"v1"}; restored := snapshot; return current.text == "v2" && restored.text == "v1" }
func observerExample() bool {
	observers := []func(int) string{func(x int) string { return fmt.Sprintf("audit:%d",x) }, func(x int) string { return fmt.Sprintf("ui:%d",x) }}
	return observers[0](7) == "audit:7" && observers[1](7) == "ui:7"
}
func stateExample() bool {
	type state int; const (loggedOut state = iota; loggedIn)
	action := func(s state) (state,string) { if s == loggedOut { return loggedIn,"login" }; return loggedOut,"logout" }
	s1,a1 := action(loggedOut); s2,a2 := action(loggedIn); return s1 == loggedIn && a1 == "login" && s2 == loggedOut && a2 == "logout"
}
func strategyExample() bool { regular := func(x int) int{return x}; discounted := func(x int) int{return x*80/100}; return regular(100)==100 && discounted(100)==80 }
func templateMethodExample() bool {
	run := func(transform func(string)string, input string) string { return "open|"+transform(input)+"|close" }
	return run(func(s string)string { r:=[]rune(s); for i,j:=0,len(r)-1;i<j;i,j=i+1,j-1 { r[i],r[j]=r[j],r[i] }; return string(r) }, "abc") == "open|cba|close"
}
func visitorExample() bool { shapes:=[]shape{circle{2}, rectangle{3,4}}; return shapes[0].area()==12 && shapes[1].area()==12 && shapes[1].perimeter()==14 }
func mvcExample() bool { model:=3; controller:=func(m int)int{return m+1}; view:=func(m int)string{return fmt.Sprintf("count=%d",m)}; return view(controller(model))=="count=4" }
func mvvmExample() bool { viewModel:=func(name string,enabled bool)(string,string){ state:="disabled"; if enabled {state="enabled"}; return "Hello "+name,state }; a,b:=viewModel("Ada",true); return a=="Hello Ada"&&b=="enabled" }
func microkernelExample() bool { plugins:=map[string]func(int)int{"double":func(x int)int{return x*2},"square":func(x int)int{return x*x}}; return plugins["double"](5)==10 }
func microservicesExample() bool { inventory:=func(s string)int{if s=="A"{return 3};return 0}; pricing:=func(s string)int{if s=="A"{return 20};return 0}; return inventory("A")==3&&pricing("A")==20 }
func enterpriseAdapterExample() bool { legacy:=func(cents int)int{return cents}; adapt:=func(dollars int)int{return legacy(dollars*100)}; return adapt(12)==1200 }
func enterpriseBridgeExample() bool { render:=func(transport func(string)string,p string)string{return transport(p)}; http:=func(p string)string{return "http:"+p}; queue:=func(p string)string{return "queue:"+p}; return render(http,"x")=="http:x"&&render(queue,"x")=="queue:x" }
func enterpriseFacadeExample() bool { validate:=func(x int)bool{return x>0}; persist:=func(x int)string{return fmt.Sprintf("saved:%d",x)}; facade:=func(x int)string{if validate(x){return persist(x)};return "rejected"}; return facade(5)=="saved:5" }
func brokerExample() bool { registry:=map[string]func(int)int{"tax":func(x int)int{return x*16/100}}; return registry["tax"](100)==16 }
func messageBusExample() bool { subscribers:=[]func(string)string{func(m string)string{return "audit:"+m},func(m string)string{return "mail:"+m}}; return subscribers[0]("paid")=="audit:paid"&&subscribers[1]("paid")=="mail:paid" }
func serviceLocatorExample() bool { services:=map[string]string{"clock":"12:00","region":"mx"}; return services["region"]=="mx" }
func activeObjectExample() bool { queue:=[]string{}; queue=append(queue,"sync"); ran:="run:"+queue[0]; queue=queue[1:]; return ran=="run:sync"&&len(queue)==0 }
func monitorObjectExample() bool { deposit:=func(amount,balance int)int{return balance+amount}; withdraw:=func(amount,balance int)int{if balance>=amount{return balance-amount};return balance}; return withdraw(7,deposit(10,5))==8 }
func halfSyncHalfAsyncExample() bool { queue:=[]string{}; queue=append(queue,"evt"); processed:="processed:"+queue[0]; queue=queue[1:]; return processed=="processed:evt"&&len(queue)==0 }
func leaderFollowersExample() bool { pool:=[]string{"a","b","c"}; leader:=pool[0]; pool=append(pool[1:],leader); return leader+":evt"=="a:evt"&&strings.Join(pool,",")=="b,c,a" }
func clientServerExample() bool { server:=func(req string)string{return "response("+req+")"}; client:=func(req string)string{return server(req)}; return client("ping")=="response(ping)" }
func peerToPeerExample() bool { send:=func(from,to,p string)string{return from+"->"+to+":"+p}; return send("a","b","x")=="a->b:x"&&send("b","a","y")=="b->a:y" }
func publishSubscribeExample() bool { subscriptions:=map[string][]string{"orders":{"audit","warehouse"},"users":{"crm"}}; return strings.Join(subscriptions["orders"],",")=="audit,warehouse" }
func distributedProxyExample() bool { remote:=func(id int)string{return fmt.Sprintf("remote-user-%d",id)}; proxy:=func(id int)string{return remote(id)}; return proxy(7)=="remote-user-7" }
func presentationAbstractionControlExample() bool { abstraction:=4; control:=func(model int,action string)int{if action=="inc"{return model+1};return model}; presentation:=func(model int)string{return fmt.Sprintf("value=%d",model)}; return presentation(control(abstraction,"inc"))=="value=5" }
func modelViewPresenterExample() bool { presenter:=func(v string)string{return "Hello "+v}; passiveView:=func(text string)string{return "["+text+"]"}; return passiveView(presenter("Ada"))=="[Hello Ada]" }
func documentViewExample() bool { d:=document{"hello"}; plain:=func(x document)string{return x.text}; upper:=func(x document)string{return strings.ToUpper(x.text)}; return plain(d)=="hello"&&upper(d)=="HELLO" }
func activeRecordExample() bool { save:=func(r record,store map[int]string){store[r.id]=r.name}; store:=map[int]string{}; save(record{1,"Ada"},store); return store[1]=="Ada" }
func dataMapperExample() bool { toRow:=func(r record)(int,string){return r.id,r.name}; fromRow:=func(id int,name string)record{return record{id,name}}; id,name:=toRow(record{1,"Ada"}); return fromRow(id,name)==(record{1,"Ada"}) }
func unitOfWorkExample() bool { pending:=[][2]string{}; pending=append(pending,[2]string{"1","Ada"}); store:=append([][2]string{},pending...); return len(store)==1&&store[0][1]=="Ada" }
func repositoryExample() bool { store:=map[int]string{}; save:=func(id int,name string){store[id]=name}; find:=func(id int)(string,bool){v,ok:=store[id];return v,ok}; save(1,"Ada"); v,ok:=find(1); return ok&&v=="Ada" }
func dependencyInjectionExample() bool { service:=func(clock func()string)string{return "time="+clock()}; return service(func()string{return "12:00"})=="time=12:00" }
func lazyInitializationExample() bool { var resource *string; created:=0; get:=func()string{if resource==nil{v:="resource";resource=&v;created++};return *resource}; return get()=="resource"&&get()=="resource"&&created==1 }
func objectPoolExample() bool { pool:=[]string{"c1","c2"}; resource:=pool[0]; pool=pool[1:]; pool=append(pool,resource); return strings.Join(pool,",")=="c2,c1" }
func nullObjectExample() bool { run:=func(logger func(string)string,msg string)string{return logger(msg)}; real:=func(m string)string{return "log:"+m}; null:=func(string)string{return ""}; return run(real,"x")=="log:x"&&run(null,"x")=="" }

func main() {
	tests := []testCase{
		{"Command",commandExample()},{"Interpreter",interpreterExample()},{"Iterator",iteratorExample()},{"Mediator",mediatorExample()},{"Memento",mementoExample()},
		{"Observer",observerExample()},{"State",stateExample()},{"Strategy",strategyExample()},{"Template Method",templateMethodExample()},{"Visitor",visitorExample()},
		{"MVC",mvcExample()},{"MVVM",mvvmExample()},{"Microkernel",microkernelExample()},{"Microservices",microservicesExample()},
		{"Enterprise Adapter",enterpriseAdapterExample()},{"Enterprise Bridge",enterpriseBridgeExample()},{"Enterprise Facade",enterpriseFacadeExample()},{"Broker",brokerExample()},{"Message Bus",messageBusExample()},{"Service Locator",serviceLocatorExample()},
		{"Active Object",activeObjectExample()},{"Monitor Object",monitorObjectExample()},{"Half-Sync / Half-Async",halfSyncHalfAsyncExample()},{"Leader / Followers",leaderFollowersExample()},
		{"Client-Server",clientServerExample()},{"Peer-to-Peer",peerToPeerExample()},{"Publish-Subscribe",publishSubscribeExample()},{"Distributed Proxy",distributedProxyExample()},
		{"Presentation-Abstraction-Control",presentationAbstractionControlExample()},{"Model-View-Presenter",modelViewPresenterExample()},{"Document-View",documentViewExample()},
		{"Active Record",activeRecordExample()},{"Data Mapper",dataMapperExample()},{"Unit of Work",unitOfWorkExample()},{"Repository",repositoryExample()},
		{"Dependency Injection",dependencyInjectionExample()},{"Lazy Initialization",lazyInitializationExample()},{"Object Pool",objectPoolExample()},{"Null Object",nullObjectExample()},
	}
	failed:=[]string{}
	for _,t:=range tests{if !t.ok{failed=append(failed,t.name)}}
	if len(failed)>0{panic("Go pattern sweep failures: "+strings.Join(failed,", "))}
	fmt.Printf("Go pattern sweep: %d/%d examples passed\n",len(tests),len(tests))
}
