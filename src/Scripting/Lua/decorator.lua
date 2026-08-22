local function plain()
  return "alert"
end

local function audit(inner)
  return function()
    return "audit(" .. inner() .. ")"
  end
end

local function encrypt(inner)
  return function()
    return "enc(" .. inner() .. ")"
  end
end

print("base=" .. plain())
print("audit=" .. audit(plain)())
print("encrypted=" .. encrypt(plain)())
print("stacked=" .. audit(encrypt(plain))())
