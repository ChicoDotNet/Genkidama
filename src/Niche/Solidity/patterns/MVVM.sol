// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library MvvmPattern {
    struct Model { uint256 amount; }
    function bindAmount(Model memory model) private pure returns (uint256) { return model.amount; }
    function run() internal pure returns (bool) {
        Model memory model = Model(10);
        uint256 beforeValue = bindAmount(model);
        model.amount += 5;
        return beforeValue == 10 && bindAmount(model) == 15;
    }
}
