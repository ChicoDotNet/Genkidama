# frozen_string_literal: true

require_relative 'patterns/command'
require_relative 'patterns/interpreter'
require_relative 'patterns/iterator'
require_relative 'patterns/mediator'
require_relative 'patterns/memento'
require_relative 'patterns/observer'
require_relative 'patterns/state'
require_relative 'patterns/strategy'
require_relative 'patterns/template_method'
require_relative 'patterns/visitor'
require_relative 'patterns/mvc'
require_relative 'patterns/mvvm'
require_relative 'patterns/microkernel'
require_relative 'patterns/microservices'
require_relative 'patterns/enterprise_adapter'
require_relative 'patterns/enterprise_bridge'
require_relative 'patterns/enterprise_facade'
require_relative 'patterns/broker'
require_relative 'patterns/message_bus'
require_relative 'patterns/service_locator'
require_relative 'patterns/active_object'
require_relative 'patterns/monitor_object'
require_relative 'patterns/half_sync_half_async'
require_relative 'patterns/leader_followers'
require_relative 'patterns/client_server'
require_relative 'patterns/peer_to_peer'
require_relative 'patterns/publish_subscribe'
require_relative 'patterns/distributed_proxy'
require_relative 'patterns/presentation_abstraction_control'
require_relative 'patterns/model_view_presenter'
require_relative 'patterns/document_view'
require_relative 'patterns/active_record'
require_relative 'patterns/data_mapper'
require_relative 'patterns/unit_of_work'
require_relative 'patterns/repository'
require_relative 'patterns/dependency_injection'
require_relative 'patterns/lazy_initialization'
require_relative 'patterns/object_pool'
require_relative 'patterns/null_object'

checks = [
  RubyPatterns::Command,
  RubyPatterns::Interpreter,
  RubyPatterns::Iterator,
  RubyPatterns::Mediator,
  RubyPatterns::Memento,
  RubyPatterns::Observer,
  RubyPatterns::State,
  RubyPatterns::Strategy,
  RubyPatterns::TemplateMethod,
  RubyPatterns::Visitor,
  RubyPatterns::MVC,
  RubyPatterns::MVVM,
  RubyPatterns::Microkernel,
  RubyPatterns::Microservices,
  RubyPatterns::EnterpriseAdapter,
  RubyPatterns::EnterpriseBridge,
  RubyPatterns::EnterpriseFacade,
  RubyPatterns::Broker,
  RubyPatterns::MessageBus,
  RubyPatterns::ServiceLocator,
  RubyPatterns::ActiveObject,
  RubyPatterns::MonitorObject,
  RubyPatterns::HalfSyncHalfAsync,
  RubyPatterns::LeaderFollowers,
  RubyPatterns::ClientServer,
  RubyPatterns::PeerToPeer,
  RubyPatterns::PublishSubscribe,
  RubyPatterns::DistributedProxy,
  RubyPatterns::PresentationAbstractionControl,
  RubyPatterns::ModelViewPresenter,
  RubyPatterns::DocumentView,
  RubyPatterns::ActiveRecord,
  RubyPatterns::DataMapper,
  RubyPatterns::UnitOfWork,
  RubyPatterns::Repository,
  RubyPatterns::DependencyInjection,
  RubyPatterns::LazyInitialization,
  RubyPatterns::ObjectPool,
  RubyPatterns::NullObject,
]

raise 'expected 39 Ruby pattern cells' unless checks.length == 39
checks.each(&:run)
puts 'ruby-pattern-sweep: 39/39 passed'
