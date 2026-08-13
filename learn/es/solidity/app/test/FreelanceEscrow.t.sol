// SPDX-License-Identifier: MIT
pragma solidity 0.8.35;

import {FreelanceEscrow} from "../src/FreelanceEscrow.sol";

interface Vm {
    function deal(address who, uint256 newBalance) external;
    function prank(address msgSender) external;
    function expectRevert(bytes4 revertData) external;
}

contract FreelanceEscrowTest {
    Vm private constant vm = Vm(address(uint160(uint256(keccak256("hevm cheat code")))));
    address private constant CLIENT = address(0xC1);
    address private constant FREELANCER = address(0xF1);
    address private constant STRANGER = address(0x51);
    uint256 private constant DEPOSIT = 1 ether;

    function setUp() public {
        vm.deal(CLIENT, 10 ether);
    }

    function _deploy() private returns (FreelanceEscrow escrow) {
        vm.prank(CLIENT);
        escrow = new FreelanceEscrow{value: DEPOSIT}(FREELANCER);
    }

    function testDepositStartsFunded() public {
        FreelanceEscrow escrow = _deploy();
        require(escrow.client() == CLIENT, "client");
        require(escrow.freelancer() == FREELANCER, "freelancer");
        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Funded), "state");
        require(address(escrow).balance == DEPOSIT, "deposit");
    }

    function testFreelancerDeliversAndClientReleases() public {
        FreelanceEscrow escrow = _deploy();
        uint256 beforeBalance = FREELANCER.balance;
        vm.prank(FREELANCER);
        escrow.markDelivered();
        vm.prank(CLIENT);
        escrow.release();
        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Released), "released");
        require(FREELANCER.balance == beforeBalance + DEPOSIT, "payment");
        require(address(escrow).balance == 0, "escrow empty");
    }

    function testClientCanRefundBeforeDelivery() public {
        FreelanceEscrow escrow = _deploy();
        uint256 beforeBalance = CLIENT.balance;
        vm.prank(CLIENT);
        escrow.refund();
        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Refunded), "refunded");
        require(CLIENT.balance == beforeBalance + DEPOSIT, "refund");
    }

    function testOnlyFreelancerCanMarkDelivered() public {
        FreelanceEscrow escrow = _deploy();
        vm.expectRevert(FreelanceEscrow.OnlyFreelancer.selector);
        vm.prank(STRANGER);
        escrow.markDelivered();
    }

    function testOnlyClientCanRelease() public {
        FreelanceEscrow escrow = _deploy();
        vm.prank(FREELANCER);
        escrow.markDelivered();
        vm.expectRevert(FreelanceEscrow.OnlyClient.selector);
        vm.prank(STRANGER);
        escrow.release();
    }

    function testCannotRefundAfterDelivery() public {
        FreelanceEscrow escrow = _deploy();
        vm.prank(FREELANCER);
        escrow.markDelivered();
        vm.expectRevert(FreelanceEscrow.InvalidState.selector);
        vm.prank(CLIENT);
        escrow.refund();
    }
}
