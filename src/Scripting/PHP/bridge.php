<?php

interface Device
{
    public function powerOn(): string;
    public function mute(): string;
}

final class TvDevice implements Device
{
    public function powerOn(): string { return 'TV:on'; }
    public function mute(): string { return 'TV:muted'; }
}

final class RadioDevice implements Device
{
    public function powerOn(): string { return 'Radio:on'; }
    public function mute(): string { return 'Radio:muted'; }
}

abstract class RemoteControl
{
    public function __construct(protected Device $device) {}
    abstract public function activate(): string;
}

final class BasicRemote extends RemoteControl
{
    public function activate(): string { return $this->device->powerOn(); }
}

final class MuteRemote extends RemoteControl
{
    public function activate(): string { return $this->device->mute(); }
}

$tv = new TvDevice();
$radio = new RadioDevice();
echo 'basic-tv=' . (new BasicRemote($tv))->activate() . PHP_EOL;
echo 'basic-radio=' . (new BasicRemote($radio))->activate() . PHP_EOL;
echo 'mute-tv=' . (new MuteRemote($tv))->activate() . PHP_EOL;
echo 'mute-radio=' . (new MuteRemote($radio))->activate() . PHP_EOL;
