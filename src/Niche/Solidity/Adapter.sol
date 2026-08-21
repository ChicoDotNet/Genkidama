// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

interface ITemperatureReader {
    function readCelsius() external view returns (int256);
}

contract LegacyFahrenheitSensor {
    function readFahrenheit() external pure returns (int256) {
        return 86;
    }
}

contract FahrenheitSensorAdapter is ITemperatureReader {
    LegacyFahrenheitSensor private immutable adaptee;

    constructor(LegacyFahrenheitSensor sensor) {
        adaptee = sensor;
    }

    function readCelsius() external view returns (int256) {
        int256 fahrenheit = adaptee.readFahrenheit();
        return ((fahrenheit - 32) * 5) / 9;
    }
}
