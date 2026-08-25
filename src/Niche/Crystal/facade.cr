class AuthService
  def authenticate(user : String) : String
    "auth(#{user})"
  end
end

class InventoryService
  def reserve(sku : String) : String
    "reserve(#{sku})"
  end
end

class BillingService
  def charge(cents : Int32) : String
    "charge(#{cents})"
  end
end

class CheckoutFacade
  def initialize(@auth : AuthService, @inventory : InventoryService, @billing : BillingService)
  end

  def checkout(user : String, sku : String, cents : Int32) : String
    [@auth.authenticate(user), @inventory.reserve(sku), @billing.charge(cents)].join(">")
  end
end

facade = CheckoutFacade.new(AuthService.new, InventoryService.new, BillingService.new)
puts "checkout=#{facade.checkout("alice", "SKU-42", 499)}"
