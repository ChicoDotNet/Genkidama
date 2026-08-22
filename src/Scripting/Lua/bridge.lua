local function device(name)
  return {
    turn_on = function() return name .. ':on' end,
    mute = function() return name .. ':muted' end
  }
end

local function remote(action)
  return function(d) return action(d) end
end

local basic = remote(function(d) return d.turn_on() end)
local mute = remote(function(d) return d.mute() end)
local tv = device('TV')
local radio = device('Radio')

print('basic-tv=' .. basic(tv))
print('basic-radio=' .. basic(radio))
print('mute-tv=' .. mute(tv))
print('mute-radio=' .. mute(radio))
