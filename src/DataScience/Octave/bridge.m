function bridge()
  tv = make_device("TV");
  radio = make_device("Radio");

  fprintf("basic-tv=%s\n", activate_basic(tv));
  fprintf("basic-radio=%s\n", activate_basic(radio));
  fprintf("mute-tv=%s\n", activate_mute(tv));
  fprintf("mute-radio=%s\n", activate_mute(radio));
end

function device = make_device(name)
  device.power_on = @() [name ":on"];
  device.mute = @() [name ":muted"];
end

function result = activate_basic(device)
  result = device.power_on();
end

function result = activate_mute(device)
  result = device.mute();
end
