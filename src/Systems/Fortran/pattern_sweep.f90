include 'patterns/command.f90'
include 'patterns/interpreter.f90'
include 'patterns/iterator.f90'
include 'patterns/mediator.f90'
include 'patterns/memento.f90'
include 'patterns/observer.f90'
include 'patterns/state.f90'
include 'patterns/strategy.f90'
include 'patterns/template_method.f90'
include 'patterns/visitor.f90'
include 'patterns/mvc.f90'
include 'patterns/mvvm.f90'
include 'patterns/microkernel.f90'
include 'patterns/microservices.f90'
include 'patterns/enterprise_adapter.f90'
include 'patterns/enterprise_bridge.f90'
include 'patterns/enterprise_facade.f90'
include 'patterns/broker.f90'
include 'patterns/message_bus.f90'
include 'patterns/service_locator.f90'
include 'patterns/active_object.f90'
include 'patterns/monitor_object.f90'
include 'patterns/half_sync_half_async.f90'
include 'patterns/leader_followers.f90'
include 'patterns/client_server.f90'
include 'patterns/peer_to_peer.f90'
include 'patterns/publish_subscribe.f90'
include 'patterns/distributed_proxy.f90'
include 'patterns/presentation_abstraction_control.f90'
include 'patterns/model_view_presenter.f90'
include 'patterns/document_view.f90'
include 'patterns/active_record.f90'
include 'patterns/data_mapper.f90'
include 'patterns/unit_of_work.f90'
include 'patterns/repository.f90'
include 'patterns/dependency_injection.f90'
include 'patterns/lazy_initialization.f90'
include 'patterns/object_pool.f90'
include 'patterns/null_object.f90'

program pattern_sweep
  use command_example, only: command_run => run
  use interpreter_example, only: interpreter_run => run
  use iterator_example, only: iterator_run => run
  use mediator_example, only: mediator_run => run
  use memento_example, only: memento_run => run
  use observer_example, only: observer_run => run
  use state_example, only: state_run => run
  use strategy_example, only: strategy_run => run
  use template_method_example, only: template_method_run => run
  use visitor_example, only: visitor_run => run
  use mvc_example, only: mvc_run => run
  use mvvm_example, only: mvvm_run => run
  use microkernel_example, only: microkernel_run => run
  use microservices_example, only: microservices_run => run
  use enterprise_adapter_example, only: enterprise_adapter_run => run
  use enterprise_bridge_example, only: enterprise_bridge_run => run
  use enterprise_facade_example, only: enterprise_facade_run => run
  use broker_example, only: broker_run => run
  use message_bus_example, only: message_bus_run => run
  use service_locator_example, only: service_locator_run => run
  use active_object_example, only: active_object_run => run
  use monitor_object_example, only: monitor_object_run => run
  use half_sync_half_async_example, only: half_sync_half_async_run => run
  use leader_followers_example, only: leader_followers_run => run
  use client_server_example, only: client_server_run => run
  use peer_to_peer_example, only: peer_to_peer_run => run
  use publish_subscribe_example, only: publish_subscribe_run => run
  use distributed_proxy_example, only: distributed_proxy_run => run
  use presentation_abstraction_control_example, only: pac_run => run
  use model_view_presenter_example, only: mvp_run => run
  use document_view_example, only: document_view_run => run
  use active_record_example, only: active_record_run => run
  use data_mapper_example, only: data_mapper_run => run
  use unit_of_work_example, only: unit_of_work_run => run
  use repository_example, only: repository_run => run
  use dependency_injection_example, only: dependency_injection_run => run
  use lazy_initialization_example, only: lazy_initialization_run => run
  use object_pool_example, only: object_pool_run => run
  use null_object_example, only: null_object_run => run
  implicit none
  logical :: checks(39)

  checks = [ &
    command_run(), interpreter_run(), iterator_run(), mediator_run(), memento_run(), &
    observer_run(), state_run(), strategy_run(), template_method_run(), visitor_run(), &
    mvc_run(), mvvm_run(), microkernel_run(), microservices_run(), enterprise_adapter_run(), &
    enterprise_bridge_run(), enterprise_facade_run(), broker_run(), message_bus_run(), service_locator_run(), &
    active_object_run(), monitor_object_run(), half_sync_half_async_run(), leader_followers_run(), client_server_run(), &
    peer_to_peer_run(), publish_subscribe_run(), distributed_proxy_run(), pac_run(), mvp_run(), &
    document_view_run(), active_record_run(), data_mapper_run(), unit_of_work_run(), repository_run(), &
    dependency_injection_run(), lazy_initialization_run(), object_pool_run(), null_object_run() ]

  if (size(checks) /= 39 .or. .not. all(checks)) error stop 'pattern sweep failed'
  print '(A)', 'Fortran pattern sweep: 39/39 examples passed'
end program
