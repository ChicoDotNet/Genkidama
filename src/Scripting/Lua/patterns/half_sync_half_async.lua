local async_queue, sync_results = {}, {}
local function submit(value) table.insert(async_queue, value) end
local function sync_worker()
  while #async_queue > 0 do
    local value = table.remove(async_queue, 1)
    table.insert(sync_results, value * 2)
  end
end
submit(2); submit(3); sync_worker()
assert(sync_results[1] == 4 and sync_results[2] == 6)
return true
