local peers = {}
local function add_peer(name) peers[name] = { inbox = {} } end
local function send(from, to, msg) table.insert(peers[to].inbox, from .. ":" .. msg) end
add_peer("a"); add_peer("b")
send("a", "b", "hello")
assert(peers.b.inbox[1] == "a:hello")
return true
