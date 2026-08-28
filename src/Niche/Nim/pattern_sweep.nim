import patterns/command_example
import patterns/interpreter_example
import patterns/iterator_example
import patterns/mediator_example
import patterns/memento_example
import patterns/observer_example
import patterns/state_example
import patterns/strategy_example
import patterns/template_method_example
import patterns/visitor_example
import patterns/mvc_example
import patterns/mvvm_example
import patterns/microkernel_example
import patterns/microservices_example
import patterns/enterprise_adapter_example
import patterns/enterprise_bridge_example
import patterns/enterprise_facade_example
import patterns/broker_example
import patterns/message_bus_example
import patterns/service_locator_example
import patterns/active_object_example
import patterns/monitor_object_example
import patterns/half_sync_half_async_example
import patterns/leader_followers_example
import patterns/client_server_example
import patterns/peer_to_peer_example
import patterns/publish_subscribe_example
import patterns/distributed_proxy_example
import patterns/presentation_abstraction_control_example
import patterns/model_view_presenter_example
import patterns/document_view_example
import patterns/active_record_example
import patterns/data_mapper_example
import patterns/unit_of_work_example
import patterns/repository_example
import patterns/dependency_injection_example
import patterns/lazy_initialization_example
import patterns/object_pool_example
import patterns/null_object_example

type PatternCheck = proc(): bool
let cases: seq[(string, PatternCheck)] = @[
  ("Command", command_example.run), ("Interpreter", interpreter_example.run), ("Iterator", iterator_example.run),
  ("Mediator", mediator_example.run), ("Memento", memento_example.run), ("Observer", observer_example.run),
  ("State", state_example.run), ("Strategy", strategy_example.run), ("Template Method", template_method_example.run),
  ("Visitor", visitor_example.run), ("MVC", mvc_example.run), ("MVVM", mvvm_example.run),
  ("Microkernel", microkernel_example.run), ("Microservices", microservices_example.run),
  ("Enterprise Adapter", enterprise_adapter_example.run), ("Enterprise Bridge", enterprise_bridge_example.run),
  ("Enterprise Facade", enterprise_facade_example.run), ("Broker", broker_example.run),
  ("Message Bus", message_bus_example.run), ("Service Locator", service_locator_example.run),
  ("Active Object", active_object_example.run), ("Monitor Object", monitor_object_example.run),
  ("Half-Sync / Half-Async", half_sync_half_async_example.run), ("Leader / Followers", leader_followers_example.run),
  ("Client-Server", client_server_example.run), ("Peer-to-Peer", peer_to_peer_example.run),
  ("Publish-Subscribe", publish_subscribe_example.run), ("Distributed Proxy", distributed_proxy_example.run),
  ("Presentation-Abstraction-Control", presentation_abstraction_control_example.run),
  ("Model-View-Presenter", model_view_presenter_example.run), ("Document-View", document_view_example.run),
  ("Active Record", active_record_example.run), ("Data Mapper", data_mapper_example.run),
  ("Unit of Work", unit_of_work_example.run), ("Repository", repository_example.run),
  ("Dependency Injection", dependency_injection_example.run), ("Lazy Initialization", lazy_initialization_example.run),
  ("Object Pool", object_pool_example.run), ("Null Object", null_object_example.run)
]
for (name, check) in cases: doAssert check(), "pattern failed: " & name
doAssert cases.len == 39
echo "Nim pattern sweep: 39/39 examples passed"
