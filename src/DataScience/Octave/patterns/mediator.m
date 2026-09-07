function mediator()
  events = {};
  routes.inventory = @inventory_receive;
  routes.payment = @payment_receive;

  payment('paid');
  inventory('reserved');
  assert(isequal(events, {'inventory<-payment:paid', 'payment<-inventory:reserved'}));

  rejected = false;
  try
    mediator_send('payment', 'unknown', 'ignored');
  catch
    rejected = true;
  end
  assert(rejected);

  function payment(message)
    mediator_send('payment', 'inventory', message);
  end

  function inventory(message)
    mediator_send('inventory', 'payment', message);
  end

  function mediator_send(sender, recipient, message)
    if ~isfield(routes, recipient)
      error('unknown colleague: %s', recipient);
    end
    receiver = routes.(recipient);
    receiver(sender, message);
  end

  function inventory_receive(sender, message)
    events{end + 1} = ['inventory<-' sender ':' message];
  end

  function payment_receive(sender, message)
    events{end + 1} = ['payment<-' sender ':' message];
  end
end
