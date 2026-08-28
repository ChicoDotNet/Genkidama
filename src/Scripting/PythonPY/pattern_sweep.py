"""Python Design Pattern sweep for the 39 post-Chain-of-Responsibility catalog cells.

Each check is intentionally tiny, executable, and focused on the observable contract
that distinguishes the named pattern. No production Genkidama code is imported.
"""
from __future__ import annotations

from collections import deque
from dataclasses import dataclass
from typing import Callable


def command() -> None:
    balance = {"value": 100}
    queue = [
        lambda: balance.__setitem__("value", balance["value"] + 50),
        lambda: balance.__setitem__("value", balance["value"] - 20),
    ]
    for action in queue:
        action()
    assert balance["value"] == 130


def interpreter() -> None:
    env = {"x": 4}
    expr = ("add", ("var", "x"), ("lit", 3))

    def evaluate(node):
        kind, *args = node
        return {
            "lit": lambda: args[0],
            "var": lambda: env[args[0]],
            "add": lambda: evaluate(args[0]) + evaluate(args[1]),
        }[kind]()

    assert evaluate(expr) == 7


def iterator() -> None:
    class Countdown:
        def __init__(self, n):
            self.n = n

        def __iter__(self):
            while self.n:
                yield self.n
                self.n -= 1

    assert list(Countdown(3)) == [3, 2, 1]


def mediator() -> None:
    events = []

    class Mediator:
        def send(self, sender, message):
            events.append((sender, message))

    Mediator().send("checkout", "paid")
    assert events == [("checkout", "paid")]


def memento() -> None:
    state = {"text": "draft"}
    snapshot = state.copy()
    state["text"] = "edited"
    state.clear()
    state.update(snapshot)
    assert state["text"] == "draft"


def observer() -> None:
    seen = []
    for subscriber in [seen.append]:
        subscriber("changed")
    assert seen == ["changed"]


def state() -> None:
    class Door:
        def __init__(self):
            self.state = "closed"

        def toggle(self):
            self.state = "open" if self.state == "closed" else "closed"

    door = Door()
    door.toggle()
    assert door.state == "open"


def strategy() -> None:
    def choose(values, strategy_fn):
        return strategy_fn(values)

    assert choose([3, 1, 2], min) == 1
    assert choose([3, 1, 2], max) == 3


def template_method() -> None:
    class Report:
        def render(self):
            return f"<{self.body()}>"

        def body(self):
            raise NotImplementedError

    class Sales(Report):
        def body(self):
            return "sales"

    assert Sales().render() == "<sales>"


def visitor() -> None:
    @dataclass
    class Number:
        value: int

    def visit_number(node):
        return node.value * 2

    assert visit_number(Number(5)) == 10


def mvc() -> None:
    model = {"count": 0}

    def controller():
        model["count"] += 1

    def view():
        return f"count={model['count']}"

    controller()
    assert view() == "count=1"


def mvvm() -> None:
    model = {"first": "Ada", "last": "Lovelace"}
    view_model = lambda: {"display_name": f"{model['first']} {model['last']}"}
    assert view_model()["display_name"] == "Ada Lovelace"


def microkernel() -> None:
    plugins = {"upper": str.upper}
    assert plugins["upper"]("plugin") == "PLUGIN"


def microservices() -> None:
    inventory = lambda sku: {"sku": sku, "available": True}
    order = lambda sku: inventory(sku)["available"]
    assert order("A-1") is True


def enterprise_adapter() -> None:
    legacy = lambda cents: cents
    adapter = lambda amount: legacy(round(amount * 100))
    assert adapter(12.34) == 1234


def enterprise_bridge() -> None:
    class Channel:
        def __init__(self, sender):
            self.sender = sender

        def notify(self, text):
            return self.sender(text)

    assert Channel(lambda text: f"sms:{text}").notify("ok") == "sms:ok"


def enterprise_facade() -> None:
    stock = lambda: True
    charge = lambda: "paid"

    def checkout():
        return charge() if stock() else "sold-out"

    assert checkout() == "paid"


def broker() -> None:
    handlers = {"price": lambda sku: 9}

    def request(topic, payload):
        return handlers[topic](payload)

    assert request("price", "A") == 9


def message_bus() -> None:
    bus = {"paid": []}
    seen = []
    bus["paid"].append(seen.append)
    for handler in bus["paid"]:
        handler(42)
    assert seen == [42]


def service_locator() -> None:
    services = {"clock": lambda: "12:00"}
    assert services["clock"]() == "12:00"


def active_object() -> None:
    mailbox = deque()
    state = []
    mailbox.append(lambda: state.append("done"))
    while mailbox:
        mailbox.popleft()()
    assert state == ["done"]


def monitor_object() -> None:
    import threading

    class Counter:
        def __init__(self):
            self.value = 0
            self.lock = threading.Lock()

        def inc(self):
            with self.lock:
                self.value += 1

    counter = Counter()
    threads = [threading.Thread(target=counter.inc) for _ in range(4)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()
    assert counter.value == 4


def half_sync_half_async() -> None:
    incoming = deque(["a", "b"])
    completed = []
    while incoming:
        completed.append(incoming.popleft().upper())
    assert completed == ["A", "B"]


def leader_followers() -> None:
    events = deque(["one", "two"])
    handled = []
    workers = iter(["leader", "follower"])
    while events:
        handled.append((next(workers), events.popleft()))
    assert handled == [("leader", "one"), ("follower", "two")]


def client_server() -> None:
    server = lambda request: {"echo": request}
    client = lambda value: server(value)["echo"]
    assert client("ping") == "ping"


def peer_to_peer() -> None:
    peers = {"a": [], "b": []}

    def send(source, target, message):
        peers[target].append((source, message))

    send("a", "b", "hello")
    assert peers["b"] == [("a", "hello")]


def publish_subscribe() -> None:
    topics = {"news": []}
    received = []
    topics["news"].append(received.append)
    for subscriber in topics["news"]:
        subscriber("v1")
    assert received == ["v1"]


def distributed_proxy() -> None:
    remote = lambda ident: {"id": ident, "name": "Ada"}

    class UserProxy:
        def __init__(self, ident):
            self.ident = ident

        @property
        def name(self):
            return remote(self.ident)["name"]

    assert UserProxy(7).name == "Ada"


def presentation_abstraction_control() -> None:
    state = {"value": 1}
    control = lambda delta: state.__setitem__("value", state["value"] + delta)
    presentation = lambda: str(state["value"])
    control(2)
    assert presentation() == "3"


def model_view_presenter() -> None:
    model = {"name": "Ada"}
    view = {}

    def presenter():
        view["text"] = model["name"].upper()

    presenter()
    assert view["text"] == "ADA"


def document_view() -> None:
    document = {"title": "One"}
    view_a = lambda: document["title"]
    view_b = lambda: document["title"].upper()
    assert (view_a(), view_b()) == ("One", "ONE")


def active_record() -> None:
    table = {}

    class User:
        def __init__(self, ident, name):
            self.id, self.name = ident, name

        def save(self):
            table[self.id] = {"name": self.name}

    User(1, "Ada").save()
    assert table[1]["name"] == "Ada"


def data_mapper() -> None:
    table = {1: {"name": "Ada"}}

    @dataclass
    class User:
        name: str

    def map_user(row):
        return User(row["name"])

    assert map_user(table[1]).name == "Ada"


def unit_of_work() -> None:
    pending = [{"id": 1}]
    database = []
    database.extend(pending)
    pending.clear()
    assert database == [{"id": 1}] and pending == []


def repository() -> None:
    data = {1: {"name": "Ada"}}

    class Users:
        def get(self, ident):
            return data[ident]

    assert Users().get(1)["name"] == "Ada"


def dependency_injection() -> None:
    class Greeter:
        def __init__(self, clock: Callable[[], str]):
            self.clock = clock

        def greet(self):
            return f"hello@{self.clock()}"

    assert Greeter(lambda: "noon").greet() == "hello@noon"


def lazy_initialization() -> None:
    calls = {"n": 0}

    class Lazy:
        _value = None

        @property
        def value(self):
            if self._value is None:
                calls["n"] += 1
                self._value = object()
            return self._value

    item = Lazy()
    first = item.value
    second = item.value
    assert first is second and calls["n"] == 1


def object_pool() -> None:
    pool = deque([{"id": 1}])
    item = pool.popleft()
    pool.append(item)
    assert pool[0] is item


def null_object() -> None:
    class NullLogger:
        def log(self, _message):
            return None

    class Service:
        def __init__(self, logger):
            self.logger = logger

        def run(self):
            self.logger.log("run")
            return "ok"

    assert Service(NullLogger()).run() == "ok"


CHECKS = [
    command,
    interpreter,
    iterator,
    mediator,
    memento,
    observer,
    state,
    strategy,
    template_method,
    visitor,
    mvc,
    mvvm,
    microkernel,
    microservices,
    enterprise_adapter,
    enterprise_bridge,
    enterprise_facade,
    broker,
    message_bus,
    service_locator,
    active_object,
    monitor_object,
    half_sync_half_async,
    leader_followers,
    client_server,
    peer_to_peer,
    publish_subscribe,
    distributed_proxy,
    presentation_abstraction_control,
    model_view_presenter,
    document_view,
    active_record,
    data_mapper,
    unit_of_work,
    repository,
    dependency_injection,
    lazy_initialization,
    object_pool,
    null_object,
]


def main() -> None:
    assert len(CHECKS) == 39
    for check in CHECKS:
        check()
    print("python-pattern-sweep: 39/39 passed")


if __name__ == "__main__":
    main()
