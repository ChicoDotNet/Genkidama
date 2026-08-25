auth_service = @(user) sprintf('auth(%s)', user);
inventory_service = @(sku) sprintf('reserve(%s)', sku);
billing_service = @(amount) sprintf('charge(%d)', amount);

steps = {
  auth_service('alice'),
  inventory_service('SKU-42'),
  billing_service(499)
};

printf('checkout=%s>%s>%s\n', steps{1}, steps{2}, steps{3});
