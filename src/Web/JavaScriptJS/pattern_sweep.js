const path = require("node:path");

const patterns = [
  "command",
  "interpreter",
  "iterator",
  "mediator",
  "memento",
  "observer",
  "state",
  "strategy",
  "template_method",
  "visitor",
  "mvc",
  "mvvm",
  "microkernel",
  "microservices",
  "enterprise_adapter",
  "enterprise_bridge",
  "enterprise_facade",
  "broker",
  "message_bus",
  "service_locator",
  "active_object",
  "monitor_object",
  "half_sync_half_async",
  "leader_followers",
  "client_server",
  "peer_to_peer",
  "publish_subscribe",
  "distributed_proxy",
  "presentation_abstraction_control",
  "model_view_presenter",
  "document_view",
  "active_record",
  "data_mapper",
  "unit_of_work",
  "repository",
  "dependency_injection",
  "lazy_initialization",
  "object_pool",
  "null_object",
];

async function main() {
  let passed = 0;
  for (const name of patterns) {
    const { run } = require(path.join(__dirname, "patterns", `${name}.js`));
    await run();
    passed += 1;
  }
  if (passed !== patterns.length) throw new Error(`expected ${patterns.length}, got ${passed}`);
  console.log(`javascript-pattern-sweep: ${passed}/${patterns.length} passed`);
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
