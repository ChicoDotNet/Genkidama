local transports = {
  email = function(msg) return "email:" .. msg end,
  sms = function(msg) return "sms:" .. msg end
}
local notification = function(transport, msg) return transport(msg) end
assert(notification(transports.sms, "ready") == "sms:ready")
return true
