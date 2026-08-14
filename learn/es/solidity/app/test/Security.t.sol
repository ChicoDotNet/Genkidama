// SPDX-License-Identifier: MIT
pragma solidity 0.8.35;

import {FreelanceEscrow} from "../src/FreelanceEscrow.sol";

interface VmSecurity {
    function deal(address who, uint256 newBalance) external;
}

contract ReentrantClient {
    FreelanceEscrow public immutable escrow;
    bool public reentrySucceeded;

    constructor(address freelancer) payable {
        escrow = new FreelanceEscrow{value: msg.value}(freelancer);
    }

    function requestRefund() external {
        escrow.refund();
    }

    receive() external payable {
        (bool success,) = address(escrow).call(abi.encodeWithSelector(FreelanceEscrow.refund.selector));
        reentrySucceeded = success;
    }
}

contract FreelanceEscrowSecurityTest {
    VmSecurity private constant vm = VmSecurity(address(uint160(uint256(keccak256("hevm cheat code")))));
    address private constant FREELANCER = address(0xF1);
    uint256 private constant DEPOSIT = 1 ether;

    function testRefundUpdatesStateBeforeExternalInteraction() public {
        vm.deal(address(this), DEPOSIT);
        ReentrantClient client = new ReentrantClient{value: DEPOSIT}(FREELANCER);
        FreelanceEscrow escrow = client.escrow();

        client.requestRefund();

        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Refunded), "refunded");
        require(address(escrow).balance == 0, "escrow empty");
        require(address(client).balance == DEPOSIT, "single refund");
        require(!client.reentrySucceeded(), "reentry rejected");
    }
}
