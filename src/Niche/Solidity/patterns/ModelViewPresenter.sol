// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library ModelViewPresenterPattern {
    struct Model { uint256 count; }
    struct View { uint256 textCount; }
    function present(Model memory model, View memory view_) private pure returns (Model memory, View memory) {
        model.count++;
        view_.textCount = model.count;
        return (model, view_);
    }
    function run() internal pure returns (bool) {
        Model memory model;
        View memory view_;
        (model, view_) = present(model, view_);
        return model.count == 1 && view_.textCount == 1;
    }
}
