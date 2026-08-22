// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

interface IDevice {
    function powerOn() external pure returns (string memory);
    function mute() external pure returns (string memory);
}

contract TvDevice is IDevice {
    function powerOn() external pure returns (string memory) { return "TV:on"; }
    function mute() external pure returns (string memory) { return "TV:muted"; }
}

contract RadioDevice is IDevice {
    function powerOn() external pure returns (string memory) { return "Radio:on"; }
    function mute() external pure returns (string memory) { return "Radio:muted"; }
}

abstract contract RemoteControl {
    IDevice internal immutable device;

    constructor(IDevice target) {
        device = target;
    }

    function activate() external view virtual returns (string memory);
}

contract BasicRemote is RemoteControl {
    constructor(IDevice target) RemoteControl(target) {}
    function activate() external view override returns (string memory) { return device.powerOn(); }
}

contract MuteRemote is RemoteControl {
    constructor(IDevice target) RemoteControl(target) {}
    function activate() external view override returns (string memory) { return device.mute(); }
}
