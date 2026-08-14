// SPDX-License-Identifier: MIT
pragma solidity 0.8.35;

/// @title FreelanceEscrow
/// @notice Educational escrow for a single freelance project.
contract FreelanceEscrow {
    enum State {
        Funded,
        Delivered,
        Released,
        Refunded
    }

    address public immutable client;
    address public immutable freelancer;
    State public state;

    error OnlyClient();
    error OnlyFreelancer();
    error InvalidState(State expected, State actual);
    error EmptyDeposit();
    error TransferFailed();

    event Delivered();
    event Released(uint256 amount);
    event Refunded(uint256 amount);

    constructor(address freelancer_) payable {
        if (msg.value == 0) revert EmptyDeposit();
        client = msg.sender;
        freelancer = freelancer_;
        state = State.Funded;
    }

    function markDelivered() external {
        if (msg.sender != freelancer) revert OnlyFreelancer();
        _requireState(State.Funded);
        state = State.Delivered;
        emit Delivered();
    }

    function release() external {
        if (msg.sender != client) revert OnlyClient();
        _requireState(State.Delivered);
        state = State.Released;
        uint256 amount = address(this).balance;
        (bool sent,) = payable(freelancer).call{value: amount}("");
        if (!sent) revert TransferFailed();
        emit Released(amount);
    }

    function refund() external {
        if (msg.sender != client) revert OnlyClient();
        _requireState(State.Funded);
        state = State.Refunded;
        uint256 amount = address(this).balance;
        (bool sent,) = payable(client).call{value: amount}("");
        if (!sent) revert TransferFailed();
        emit Refunded(amount);
    }

    function _requireState(State expected) private view {
        if (state != expected) revert InvalidState(expected, state);
    }
}
