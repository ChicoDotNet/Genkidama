class AuthService
  def authenticate(user) = "auth(#{user})"
end

class InventoryService
  def reserve(sku) = "reserve(#{sku})"
end

class BillingService
  def charge(cents) = "charge(#{cents})"
end

class CheckoutFacade
  def initialize(auth:, inventory:, billing:)
    @auth = auth
    @inventory = inventory
    @billing = billing
  end

  def checkout(user, sku, cents)
    "checkout=#{@auth.authenticate(user)}>#{@inventory.reserve(sku)}>#{@billing.charge(cents)}"
  end
end

facade = CheckoutFacade.new(auth: AuthService.new, inventory: InventoryService.new, billing: BillingService.new)
puts facade.checkout('alice', 'SKU-42', 499)
