function bridge
    tv = makeDevice("TV");
    radio = makeDevice("Radio");

    fprintf('basic-tv=%s\n', activateBasic(tv));
    fprintf('basic-radio=%s\n', activateBasic(radio));
    fprintf('mute-tv=%s\n', activateMute(tv));
    fprintf('mute-radio=%s\n', activateMute(radio));
end

function device = makeDevice(name)
    device.powerOn = @() name + ":on";
    device.mute = @() name + ":muted";
end

function value = activateBasic(device)
    value = device.powerOn();
end

function value = activateMute(device)
    value = device.mute();
end
