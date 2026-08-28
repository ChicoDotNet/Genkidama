local account = { balance = 100 }
local queue = {
  function() account.balance = account.balance + 50 end,
  function() account.balance = account.balance - 20 end
}
for _, command in ipairs(queue) do command() end
assert(account.balance == 130)
return true
