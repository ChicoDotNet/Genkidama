function validate_pattern_sweep
%VALIDATE_PATTERN_SWEEP Validate all remaining MATLAB pattern examples in one runtime.

r = command;
assert(r.queue == "deposit(50)>withdraw(20)");
assert(r.executed == "deposit>withdraw");
assert(r.balance == 130);
assert(r.balanceAfterUndo == 150);

r = interpreter;
assert(r.value == 19);

r = iterator;
assert(isequal(r.visited, [10 20 30]));
assert(r.finished);

r = mediator;
assert(r.events == "inventory<-payment:paid>payment<-inventory:reserved");
assert(r.rejectedUnknown);

r = memento;
assert(r.changed == "published");
assert(r.restored == "draft");

r = observer;
assert(r.notifications == "audit:42>dashboard:42");

r = state;
assert(r.sequence == "locked>unlocked>locked");
assert(r.actions == "unlock>lock");

r = strategy;
assert(r.regular == 100);
assert(r.vip == 80);

r = template_method;
assert(r.csv == "read-csv>normalize>publish");
assert(r.json == "read-json>aggregate>publish");

r = visitor;
assert(abs(r.areaTotal - (4*pi + 12)) < 1e-10);
assert(r.labels == "circle>rectangle");

r = mvc;
assert(r.before == "count=0");
assert(r.after == "count=1");
assert(r.count == 1);

r = mvvm;
assert(r.before == "$10.00");
assert(r.after == "$15.00");
assert(r.amount == 15);

r = microkernel;
assert(r.double == 8);
assert(r.square == 16);

r = microservices;
assert(r.reserved);
assert(r.remaining == 5);
assert(r.status == "confirmed");

r = enterprise_adapter;
assert(r.customerId == 17);
assert(r.amount == 12.5);

r = enterprise_bridge;
assert(r.alert == "kafka>ALERT:disk");
assert(r.reminder == "queue>REMINDER:backup");

r = enterprise_facade;
assert(r.trace == "crm:create:77>billing:open:77");

r = broker;
assert(r.inventory == "inventory:sku-1=7");
assert(r.customer == "customer:17=active");

r = message_bus;
assert(r.deliveries == "audit:order-created:42>billing:order-created:42");

r = service_locator;
assert(r.email == "email>a@example.test");
assert(r.audit == "audit>created");

r = active_object;
assert(r.before == 0);
assert(r.after == 12);
assert(r.trace == "add>multiply");

r = monitor_object;
assert(r.value == 5);
assert(r.observations == "5");
assert(~r.locked);
assert(r.maxCritical == 1);

r = half_sync_half_async;
assert(r.queued == "job-1>job-2>job-3");
assert(r.processed == "done:job-1>done:job-2>done:job-3");

r = leader_followers;
assert(r.handled == "worker-1:event-a>worker-2:event-b>worker-3:event-c");
assert(r.nextLeader == "worker-1");

r = client_server;
assert(r.status == 200);
assert(r.body == "stock=7");

r = peer_to_peer;
assert(r.origin == "peer-a");
assert(r.deliveries == "peer-a>peer-b:block-42>peer-a>peer-c:block-42");

r = publish_subscribe;
assert(r.received == "warehouse:51>analytics:51");

r = distributed_proxy;
assert(r.stock == 7);
assert(r.source == "remote");

r = presentation_abstraction_control;
assert(r.childView == "child:view=42");
assert(r.rootView == "root:view=42");

r = model_view_presenter;
assert(r.count == 1);
assert(r.viewText == "count=1");

r = document_view;
assert(r.editor == "editor:Final:120");
assert(r.summary == "summary:Final");

r = active_record;
assert(r.id == 7);
assert(r.name == "Ada");

r = data_mapper;
assert(r.rowKey == "person:8");
assert(r.domainName == "Grace");

r = unit_of_work;
assert(isequal(r.before, [10 20]));
assert(isequal(r.after, [15 17]));
assert(r.committed);

r = repository;
assert(r.id == 9);
assert(r.name == "Linus");

r = dependency_injection;
assert(r.production == "smtp:Ada");
assert(r.test == "fake:Ada");

r = lazy_initialization;
assert(r.first == "resource-ready");
assert(r.second == "resource-ready");
assert(r.creationCount == 1);

r = object_pool;
assert(r.first == 1);
assert(r.second == 2);
assert(r.reused == 1);

r = null_object;
assert(r.real == "logged:processed:item-1");
assert(r.null == "");

fprintf('MATLAB pattern sweep: 39/39 examples passed\n');
end
