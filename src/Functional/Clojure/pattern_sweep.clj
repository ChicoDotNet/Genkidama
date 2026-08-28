(ns pattern-sweep)
(doseq [file [
  "command.clj" "interpreter.clj" "iterator.clj" "mediator.clj" "memento.clj" "observer.clj" "state.clj" "strategy.clj" "template_method.clj" "visitor.clj"
  "mvc.clj" "mvvm.clj" "microkernel.clj" "microservices.clj" "enterprise_adapter.clj" "enterprise_bridge.clj" "enterprise_facade.clj" "broker.clj" "message_bus.clj" "service_locator.clj"
  "active_object.clj" "monitor_object.clj" "half_sync_half_async.clj" "leader_followers.clj" "client_server.clj" "peer_to_peer.clj" "publish_subscribe.clj" "distributed_proxy.clj"
  "presentation_abstraction_control.clj" "model_view_presenter.clj" "document_view.clj" "active_record.clj" "data_mapper.clj" "unit_of_work.clj" "repository.clj" "dependency_injection.clj"
  "lazy_initialization.clj" "object_pool.clj" "null_object.clj"]]
  (load-file (str "src/Functional/Clojure/patterns/" file)))
(def cases [command-pattern interpreter-pattern iterator-pattern mediator-pattern memento-pattern observer-pattern state-pattern strategy-pattern template-method-pattern visitor-pattern mvc-pattern mvvm-pattern microkernel-pattern microservices-pattern enterprise-adapter-pattern enterprise-bridge-pattern enterprise-facade-pattern broker-pattern message-bus-pattern service-locator-pattern active-object-pattern monitor-object-pattern half-sync-half-async-pattern leader-followers-pattern client-server-pattern peer-to-peer-pattern publish-subscribe-pattern distributed-proxy-pattern pac-pattern mvp-pattern document-view-pattern active-record-pattern data-mapper-pattern unit-of-work-pattern repository-pattern dependency-injection-pattern lazy-initialization-pattern object-pool-pattern null-object-pattern])
(doseq [f cases] (assert (f)))
(assert (= 39 (count cases)))
(println "Clojure pattern sweep: 39/39 examples passed")
