<?php declare(strict_types=1);
$patterns = [
'command','interpreter','iterator','mediator','memento','observer','state','strategy','template_method','visitor','mvc','mvvm','microkernel','microservices','enterprise_adapter','enterprise_bridge','enterprise_facade','broker','message_bus','service_locator','active_object','monitor_object','half_sync_half_async','leader_followers','client_server','peer_to_peer','publish_subscribe','distributed_proxy','presentation_abstraction_control','model_view_presenter','document_view','active_record','data_mapper','unit_of_work','repository','dependency_injection','lazy_initialization','object_pool','null_object'
];
$passed=0;
foreach ($patterns as $pattern) { require __DIR__."/patterns/$pattern.php"; $passed++; }
if ($passed !== 39) throw new RuntimeException("expected 39, got $passed");
echo "php-pattern-sweep: $passed/39 passed\n";
