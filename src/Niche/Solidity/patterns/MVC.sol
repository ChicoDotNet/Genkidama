// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MvcPattern {
    struct Model { uint256 count; }
    function controllerIncrement(Model memory model) private pure returns (Model memory) { model.count++; return model; }
    function view(Model memory model) private pure returns (uint256) { return model.count; }
    function run() internal pure returns (bool) {
        Model memory model = Model(0);
        uint256 beforeValue = view(model);
        model = controllerIncrement(model);
        return beforeValue == 0 && view(model) == 1;
    }
}
