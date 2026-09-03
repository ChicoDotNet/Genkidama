package main

import (
	"fmt"
	"math"
	"sync"
)

func must(ok bool) {
	if !ok {
		panic("pattern assertion failed")
	}
}

// Command: requests are reified as executable/undoable values.
type balanceCommand struct {
	delta int
	name  string
}

func (c balanceCommand) execute(balance int) int { return balance + c.delta }
func (c balanceCommand) undo(balance int) int    { return balance - c.delta }
func commandPattern() {
	b := 100
	q := []balanceCommand{{50, "deposit"}, {-20, "withdraw"}}
	trace := ""
	for i, c := range q {
		b = c.execute(b)
		if i > 0 {
			trace += ">"
		}
		trace += c.name
	}
	must(b == 130 && trace == "deposit>withdraw")
	b = q[1].undo(b)
	must(b == 150)
}

// Interpreter: recursive AST nodes interpret a tiny arithmetic grammar.
type expr interface{ eval() int }
type lit int

func (n lit) eval() int { return int(n) }

type add struct{ a, b expr }

func (x add) eval() int { return x.a.eval() + x.b.eval() }

type mul struct{ a, b expr }

func (x mul) eval() int   { return x.a.eval() * x.b.eval() }
func interpreterPattern() { must(add{lit(7), mul{lit(3), lit(4)}}.eval() == 19) }

// Iterator: explicit cursor hides traversal mechanics.
type intIterator struct {
	values []int
	index  int
}

func (it *intIterator) next() (int, bool) {
	if it.index >= len(it.values) {
		return 0, false
	}
	v := it.values[it.index]
	it.index++
	return v, true
}
func iteratorPattern() {
	it := &intIterator{values: []int{10, 20, 30}}
	got := []int{}
	for {
		v, ok := it.next()
		if !ok {
			break
		}
		got = append(got, v)
	}
	_, ok := it.next()
	must(!ok && fmt.Sprint(got) == "[10 20 30]")
}

// Mediator: colleagues communicate only through the mediator.
type mediator struct{ events []string }

func (m *mediator) notify(sender, event string) {
	if sender == "button" && event == "click" {
		m.events = append(m.events, "panel.refresh")
	}
	if sender == "panel" && event == "loaded" {
		m.events = append(m.events, "button.enable")
	}
}
func mediatorPattern() {
	m := &mediator{}
	m.notify("button", "click")
	m.notify("panel", "loaded")
	must(fmt.Sprint(m.events) == "[panel.refresh button.enable]")
}

// Memento: state snapshot is opaque to the caretaker.
type editor struct{ state string }
type editorMemento struct{ state string }

func (e editor) save() editorMemento      { return editorMemento{e.state} }
func (e *editor) restore(m editorMemento) { e.state = m.state }
func mementoPattern() {
	e := editor{"draft"}
	m := e.save()
	e.state = "published"
	must(e.state == "published")
	e.restore(m)
	must(e.state == "draft")
}

// Observer: the sweep delegates to the individually addressable canonical example.
func observerPattern() { must(observerExamplePasses()) }

// State: behavior/transition are delegated to the current state value.
type gateState string

func transition(s gateState, action string) gateState {
	if s == "locked" && action == "unlock" {
		return "unlocked"
	}
	if s == "unlocked" && action == "lock" {
		return "locked"
	}
	return s
}
func statePattern() {
	s := gateState("locked")
	s = transition(s, "unlock")
	must(s == "unlocked")
	s = transition(s, "lock")
	must(s == "locked")
}

// Strategy: algorithm is supplied independently of the context.
func price(base int, strategy func(int) int) int { return strategy(base) }
func strategyPattern() {
	regular := func(v int) int { return v }
	vip := func(v int) int { return v * 80 / 100 }
	must(price(100, regular) == 100 && price(100, vip) == 80)
}

// Template Method: fixed skeleton calls variable steps.
func pipeline(read string, transform func() string) string {
	return read + ">" + transform() + ">publish"
}
func templateMethodPattern() {
	must(pipeline("read-csv", func() string { return "normalize" }) == "read-csv>normalize>publish")
	must(pipeline("read-json", func() string { return "aggregate" }) == "read-json>aggregate>publish")
}

// Visitor: operations are separate visitors over a stable shape hierarchy.
type shape interface {
	accept(shapeVisitor) float64
	label() string
}
type shapeVisitor interface {
	visitCircle(circle) float64
	visitRectangle(rectangle) float64
}
type circle struct{ r float64 }
type rectangle struct{ w, h float64 }

func (c circle) accept(v shapeVisitor) float64    { return v.visitCircle(c) }
func (c circle) label() string                    { return "circle" }
func (r rectangle) accept(v shapeVisitor) float64 { return v.visitRectangle(r) }
func (r rectangle) label() string                 { return "rectangle" }

type areaVisitor struct{}

func (areaVisitor) visitCircle(c circle) float64       { return math.Pi * c.r * c.r }
func (areaVisitor) visitRectangle(r rectangle) float64 { return r.w * r.h }
func visitorPattern() {
	v := areaVisitor{}
	shapes := []shape{circle{2}, rectangle{3, 4}}
	total := 0.0
	labels := ""
	for i, s := range shapes {
		total += s.accept(v)
		if i > 0 {
			labels += ">"
		}
		labels += s.label()
	}
	must(math.Abs(total-(4*math.Pi+12)) < 1e-9 && labels == "circle>rectangle")
}

// MVC: controller mutates model; view projects it.
type counterModel struct{ count int }
type counterController struct{ m *counterModel }

func (c counterController) increment()    { c.m.count++ }
func renderCounter(m counterModel) string { return fmt.Sprintf("count=%d", m.count) }
func mvcPattern() {
	m := &counterModel{}
	before := renderCounter(*m)
	counterController{m}.increment()
	must(before == "count=0" && renderCounter(*m) == "count=1")
}

// MVVM: view-model exposes presentation state and commands.
type amountVM struct{ amount int }

func (v amountVM) text() string { return fmt.Sprintf("$%d.00", v.amount) }
func (v *amountVM) add(n int)   { v.amount += n }
func mvvmPattern() {
	v := &amountVM{10}
	before := v.text()
	v.add(5)
	must(before == "$10.00" && v.text() == "$15.00")
}

// Microkernel: tiny core dispatches registered plugins.
type kernel struct{ plugins map[string]func(int) int }

func (k *kernel) register(n string, f func(int) int) { k.plugins[n] = f }
func (k kernel) run(n string, v int) int             { return k.plugins[n](v) }
func microkernelPattern() {
	k := kernel{map[string]func(int) int{}}
	k.register("double", func(v int) int { return v * 2 })
	k.register("square", func(v int) int { return v * v })
	must(k.run("double", 4) == 8 && k.run("square", 4) == 16)
}

// Microservices: inventory and order services collaborate through explicit contracts.
type inventoryService struct{ stock int }

func (i *inventoryService) reserve(q int) bool {
	if q > i.stock {
		return false
	}
	i.stock -= q
	return true
}

type orderService struct{ inventory *inventoryService }

func (o orderService) place(q int) string {
	if o.inventory.reserve(q) {
		return "confirmed"
	}
	return "rejected"
}
func microservicesPattern() {
	i := &inventoryService{7}
	status := orderService{i}.place(2)
	must(status == "confirmed" && i.stock == 5)
}

// Enterprise Adapter: legacy tuple is translated to canonical domain data.
type legacyCustomer struct {
	code  int
	cents int
}
type canonicalCustomer struct {
	id     int
	amount float64
}

func adaptCustomer(x legacyCustomer) canonicalCustomer {
	return canonicalCustomer{x.code, float64(x.cents) / 100}
}
func enterpriseAdapterPattern() {
	c := adaptCustomer(legacyCustomer{17, 1250})
	must(c.id == 17 && c.amount == 12.5)
}

// Enterprise Bridge: abstraction and transport vary independently.
type transport interface{ send(string) string }
type namedTransport string

func (t namedTransport) send(msg string) string       { return string(t) + ">" + msg }
func sendNotice(kind, msg string, t transport) string { return t.send(kind + ":" + msg) }
func enterpriseBridgePattern() {
	must(sendNotice("ALERT", "disk", namedTransport("kafka")) == "kafka>ALERT:disk")
	must(sendNotice("REMINDER", "backup", namedTransport("queue")) == "queue>REMINDER:backup")
}

// Enterprise Facade: one operation coordinates several integration subsystems.
func enterpriseFacadePattern() {
	crm := func(id int) string { return fmt.Sprintf("crm:create:%d", id) }
	billing := func(id int) string { return fmt.Sprintf("billing:open:%d", id) }
	must(crm(77)+">"+billing(77) == "crm:create:77>billing:open:77")
}

// Broker: caller asks intermediary instead of knowing service locations.
type brokerRegistry map[string]func(string) string

func brokerPattern() {
	b := brokerRegistry{"inventory": func(k string) string { return "inventory:" + k + "=7" }, "customer": func(k string) string { return "customer:" + k + "=active" }}
	must(b["inventory"]("sku-1") == "inventory:sku-1=7" && b["customer"]("17") == "customer:17=active")
}

// Message Bus: handlers consume a common message envelope.
type message struct {
	topic string
	id    int
}
type messageBus struct{ handlers []func(message) string }

func (b *messageBus) on(f func(message) string) { b.handlers = append(b.handlers, f) }
func (b messageBus) send(m message) []string {
	out := []string{}
	for _, f := range b.handlers {
		out = append(out, f(m))
	}
	return out
}
func messageBusPattern() {
	b := messageBus{}
	b.on(func(m message) string { return fmt.Sprintf("audit:%s:%d", m.topic, m.id) })
	b.on(func(m message) string { return fmt.Sprintf("billing:%s:%d", m.topic, m.id) })
	must(fmt.Sprint(b.send(message{"order-created", 42})) == "[audit:order-created:42 billing:order-created:42]")
}

// Service Locator: dependencies are resolved at runtime from a registry.
func serviceLocatorPattern() {
	loc := map[string]func(string) string{"email": func(v string) string { return "email>" + v }, "audit": func(v string) string { return "audit>" + v }}
	must(loc["email"]("a@example.test") == "email>a@example.test" && loc["audit"]("created") == "audit>created")
}

// Active Object: invocation queues commands; scheduler executes later.
func activeObjectPattern() {
	value := 0
	q := []func(){func() { value += 3 }, func() { value *= 4 }}
	before := value
	for _, f := range q {
		f()
	}
	must(before == 0 && value == 12)
}

// Monitor Object: synchronization is encapsulated with the state.
type monitoredCounter struct {
	mu          sync.Mutex
	value       int
	maxCritical int
	critical    int
}

func (c *monitoredCounter) add(n int) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.critical++
	if c.critical > c.maxCritical {
		c.maxCritical = c.critical
	}
	c.value += n
	c.critical--
}
func monitorObjectPattern() {
	c := &monitoredCounter{}
	var wg sync.WaitGroup
	for _, n := range []int{2, 3} {
		wg.Add(1)
		go func(v int) { defer wg.Done(); c.add(v) }(n)
	}
	wg.Wait()
	must(c.value == 5 && c.maxCritical == 1)
}

// Half-Sync/Half-Async: arrivals enqueue; synchronous layer drains in order.
func halfSyncHalfAsyncPattern() {
	queue := []string{"job-1", "job-2", "job-3"}
	out := []string{}
	for _, j := range queue {
		out = append(out, "done:"+j)
	}
	must(fmt.Sprint(out) == "[done:job-1 done:job-2 done:job-3]")
}

// Leader/Followers: one leader handles an event then hands leadership to a follower.
func leaderFollowersPattern() {
	workers := []string{"worker-1", "worker-2", "worker-3"}
	events := []string{"event-a", "event-b", "event-c"}
	handled := []string{}
	for i, e := range events {
		handled = append(handled, workers[i]+":"+e)
	}
	next := workers[len(events)%len(workers)]
	must(fmt.Sprint(handled) == "[worker-1:event-a worker-2:event-b worker-3:event-c]" && next == "worker-1")
}

// Client-Server: client request is separated from centralized server handling.
type request struct{ key string }
type response struct {
	status int
	body   string
}

func serverHandle(r request) response {
	if r.key == "sku-1" {
		return response{200, "stock=7"}
	}
	return response{404, "missing"}
}
func clientServerPattern() {
	r := serverHandle(request{"sku-1"})
	must(r.status == 200 && r.body == "stock=7")
}

// Peer-to-Peer: the same peer type can originate and receive data.
type peer struct {
	name  string
	inbox []string
}

func (p *peer) send(other *peer, data string) {
	other.inbox = append(other.inbox, p.name+">"+other.name+":"+data)
}
func peerToPeerPattern() {
	a := &peer{name: "peer-a"}
	b := &peer{name: "peer-b"}
	c := &peer{name: "peer-c"}
	a.send(b, "block-42")
	a.send(c, "block-42")
	must(fmt.Sprint(append(b.inbox, c.inbox...)) == "[peer-a>peer-b:block-42 peer-a>peer-c:block-42]")
}

// Publish-Subscribe: publisher targets a topic, not subscribers.
type pubsub struct{ subs map[string][]func(int) string }

func (p *pubsub) subscribe(topic string, f func(int) string) {
	p.subs[topic] = append(p.subs[topic], f)
}
func (p pubsub) publish(topic string, id int) []string {
	out := []string{}
	for _, f := range p.subs[topic] {
		out = append(out, f(id))
	}
	return out
}
func publishSubscribePattern() {
	p := pubsub{subs: map[string][]func(int) string{}}
	p.subscribe("order", func(id int) string { return fmt.Sprintf("warehouse:%d", id) })
	p.subscribe("order", func(id int) string { return fmt.Sprintf("analytics:%d", id) })
	must(fmt.Sprint(p.publish("order", 51)) == "[warehouse:51 analytics:51]")
}

// Distributed Proxy: local object preserves remote contract while hiding transport.
type stockService interface{ stock(string) int }
type remoteStock struct{}

func (remoteStock) stock(_ string) int { return 7 }

type stockProxy struct{ remote stockService }

func (p stockProxy) stock(s string) int { return p.remote.stock(s) }
func distributedProxyPattern()          { p := stockProxy{remoteStock{}}; must(p.stock("sku-1") == 7) }

// PAC: each agent separates presentation, abstraction and control.
type pacAgent struct {
	name  string
	value int
}

func (a pacAgent) view() string { return fmt.Sprintf("%s:view=%d", a.name, a.value) }
func presentationAbstractionControlPattern() {
	root := pacAgent{"root", 42}
	child := pacAgent{"child", 42}
	must(child.view() == "child:view=42" && root.view() == "root:view=42")
}

// MVP: presenter updates passive view from model.
type passiveView struct{ text string }
type presenter struct {
	model *counterModel
	view  *passiveView
}

func (p presenter) increment() { p.model.count++; p.view.text = renderCounter(*p.model) }
func modelViewPresenterPattern() {
	m := &counterModel{}
	v := &passiveView{}
	presenter{m, v}.increment()
	must(m.count == 1 && v.text == "count=1")
}

// Document-View: independent views project one shared document.
type document struct {
	title string
	words int
}

func editorView(d document) string  { return fmt.Sprintf("editor:%s:%d", d.title, d.words) }
func summaryView(d document) string { return "summary:" + d.title }
func documentViewPattern() {
	d := document{"Final", 120}
	must(editorView(d) == "editor:Final:120" && summaryView(d) == "summary:Final")
}

// Active Record: domain record owns persistence operations.
type personRecord struct {
	id   int
	name string
}

var personTable = map[int]personRecord{}

func (p personRecord) save()         { personTable[p.id] = p }
func loadPerson(id int) personRecord { return personTable[id] }
func activeRecordPattern() {
	personRecord{7, "Ada"}.save()
	p := loadPerson(7)
	must(p.id == 7 && p.name == "Ada")
}

// Data Mapper: mapper isolates persistence representation from domain object.
type person struct {
	id   int
	name string
}
type row map[string]any

func toRow(p person) row   { return row{"key": fmt.Sprintf("person:%d", p.id), "name": p.name} }
func fromRow(r row) person { return person{8, r["name"].(string)} }
func dataMapperPattern() {
	r := toRow(person{8, "Grace"})
	p := fromRow(r)
	must(r["key"] == "person:8" && p.name == "Grace")
}

// Unit of Work: changes are staged then committed as one unit.
type unitOfWork struct {
	values []int
	deltas []int
}

func (u *unitOfWork) stage(i, delta int) {
	for len(u.deltas) <= i {
		u.deltas = append(u.deltas, 0)
	}
	u.deltas[i] += delta
}
func (u *unitOfWork) commit() {
	for i, d := range u.deltas {
		u.values[i] += d
	}
	u.deltas = nil
}
func unitOfWorkPattern() {
	u := &unitOfWork{values: []int{10, 20}}
	before := append([]int(nil), u.values...)
	u.stage(0, 5)
	u.stage(1, -3)
	u.commit()
	must(fmt.Sprint(before) == "[10 20]" && fmt.Sprint(u.values) == "[15 17]")
}

// Repository: collection-like domain access hides storage details.
type personRepo struct{ items map[int]person }

func (r personRepo) byID(id int) (person, bool) { p, ok := r.items[id]; return p, ok }
func repositoryPattern() {
	r := personRepo{map[int]person{9: {9, "Linus"}}}
	p, ok := r.byID(9)
	must(ok && p.name == "Linus")
}

// Dependency Injection: dependency is supplied from outside the consumer.
type greeter struct{ send func(string) string }

func (g greeter) greet(n string) string { return g.send(n) }
func dependencyInjectionPattern() {
	prod := greeter{func(n string) string { return "smtp:" + n }}
	test := greeter{func(n string) string { return "fake:" + n }}
	must(prod.greet("Ada") == "smtp:Ada" && test.greet("Ada") == "fake:Ada")
}

// Lazy Initialization: expensive value is created on first access and reused.
type lazyResource struct {
	once      sync.Once
	value     string
	creations int
}

func (l *lazyResource) get() string {
	l.once.Do(func() { l.value = "resource-ready"; l.creations++ })
	return l.value
}
func lazyInitializationPattern() {
	l := &lazyResource{}
	must(l.get() == "resource-ready" && l.get() == "resource-ready" && l.creations == 1)
}

// Object Pool: bounded reusable resources are acquired/released.
type objectPool struct {
	available []int
	next      int
}

func (p *objectPool) acquire() int {
	if len(p.available) > 0 {
		v := p.available[len(p.available)-1]
		p.available = p.available[:len(p.available)-1]
		return v
	}
	p.next++
	return p.next
}
func (p *objectPool) release(v int) { p.available = append(p.available, v) }
func objectPoolPattern() {
	p := &objectPool{}
	first := p.acquire()
	second := p.acquire()
	p.release(first)
	reused := p.acquire()
	must(first == 1 && second == 2 && reused == 1)
}

// Null Object: no-op implementation preserves collaborator contract.
type logger interface{ log(string) string }
type realLogger struct{}
type nullLogger struct{}

func (realLogger) log(v string) string { return "logged:" + v }
func (nullLogger) log(string) string   { return "" }
func nullObjectPattern() {
	must(realLogger{}.log("processed:item-1") == "logged:processed:item-1" && nullLogger{}.log("processed:item-1") == "")
}

func main() {
	cases := []func(){commandPattern, interpreterPattern, iteratorPattern, mediatorPattern, mementoPattern, observerPattern, statePattern, strategyPattern, templateMethodPattern, visitorPattern, mvcPattern, mvvmPattern, microkernelPattern, microservicesPattern, enterpriseAdapterPattern, enterpriseBridgePattern, enterpriseFacadePattern, brokerPattern, messageBusPattern, serviceLocatorPattern, activeObjectPattern, monitorObjectPattern, halfSyncHalfAsyncPattern, leaderFollowersPattern, clientServerPattern, peerToPeerPattern, publishSubscribePattern, distributedProxyPattern, presentationAbstractionControlPattern, modelViewPresenterPattern, documentViewPattern, activeRecordPattern, dataMapperPattern, unitOfWorkPattern, repositoryPattern, dependencyInjectionPattern, lazyInitializationPattern, objectPoolPattern, nullObjectPattern}
	must(len(cases) == 39)
	for _, c := range cases {
		c()
	}
	fmt.Println("Go pattern sweep: 39/39 examples passed")
}
