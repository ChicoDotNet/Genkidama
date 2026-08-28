local files = {
  "command.lua", "interpreter.lua", "iterator.lua", "mediator.lua", "memento.lua", "observer.lua", "state.lua", "strategy.lua", "template_method.lua", "visitor.lua",
  "mvc.lua", "mvvm.lua", "microkernel.lua", "microservices.lua", "enterprise_adapter.lua", "enterprise_bridge.lua", "enterprise_facade.lua", "broker.lua", "message_bus.lua", "service_locator.lua",
  "active_object.lua", "monitor_object.lua", "half_sync_half_async.lua", "leader_followers.lua", "client_server.lua", "peer_to_peer.lua", "publish_subscribe.lua", "distributed_proxy.lua",
  "presentation_abstraction_control.lua", "model_view_presenter.lua", "document_view.lua", "active_record.lua", "data_mapper.lua", "unit_of_work.lua", "repository.lua",
  "dependency_injection.lua", "lazy_initialization.lua", "object_pool.lua", "null_object.lua"
}
local base = "src/Scripting/Lua/patterns/"
local passed = 0
for _, file in ipairs(files) do
  local ok, result = pcall(dofile, base .. file)
  assert(ok, file .. ": " .. tostring(result))
  assert(result == true, file .. ": expected true")
  passed = passed + 1
end
assert(passed == 39)
print(string.format("lua-pattern-sweep: %d/39 passed", passed))
