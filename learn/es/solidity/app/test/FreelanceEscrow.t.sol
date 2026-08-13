// SPDX-License-Identifier: MIT
pragma solidity 0.8.35;

import {FreelanceEscrow} from "../src/FreelanceEscrow.sol";

interface Vm {
    function deal(address who, uint256 newBalance) external;
    function prank(address msgSender) external;
    function expectRevert(bytes4 revertData) external;
    function expectRevert(bytes calldata revertData) external;
}

contract RejectingFreelancer {
    function deliver(FreelanceEscrow escrow) external {
        escrow.markDelivered();
    }

    receive() external payable {
        revert("reject payment");
    }
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

    function _deployWithAmount(uint256 amount) private returns (FreelanceEscrow escrow) {
        vm.deal(CLIENT, amount);
        vm.prank(CLIENT);
        escrow = new FreelanceEscrow{value: amount}(FREELANCER);
    }

    function testDepositStartsFunded() public {
        FreelanceEscrow escrow = _deploy();
        require(escrow.client() == CLIENT, "client");
        require(escrow.freelancer() == FREELANCER, "freelancer");
        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Funded), "state");
        require(address(escrow).balance == DEPOSIT, "deposit");
    }

    function testFuzzPositiveDepositStartsFunded(uint96 rawAmount) public {
        uint256 amount = uint256(rawAmount);
        if (amount == 0) {
            return;
        }

        FreelanceEscrow escrow = _deployWithAmount(amount);

        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Funded), "state");
        require(address(escrow).balance == amount, "exact deposit");
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

    function testFuzzReleaseTransfersExactDeposit(uint96 rawAmount) public {
        uint256 amount = uint256(rawAmount);
        if (amount == 0) {
            return;
        }

        FreelanceEscrow escrow = _deployWithAmount(amount);
        uint256 beforeBalance = FREELANCER.balance;

        vm.prank(FREELANCER);
        escrow.markDelivered();
        vm.prank(CLIENT);
        escrow.release();

        require(FREELANCER.balance == beforeBalance + amount, "exact payment");
        require(address(escrow).balance == 0, "escrow empty");
    }

    function testReleaseFailureRevertsStateAndRetainsFunds() public {
        RejectingFreelancer rejecting = new RejectingFreelancer();
        vm.prank(CLIENT);
        FreelanceEscrow escrow = new FreelanceEscrow{value: DEPOSIT}(address(rejecting));
        rejecting.deliver(escrow);

        vm.expectRevert(FreelanceEscrow.TransferFailed.selector);
        vm.prank(CLIENT);
        escrow.release();

        require(
            uint256(escrow.state()) == uint256(FreelanceEscrow.State.Delivered), "state rolled back"
        );
        require(address(escrow).balance == DEPOSIT, "funds retained");
    }

    function testClientCanRefundBeforeDelivery() public {
        FreelanceEscrow escrow = _deploy();
        uint256 beforeBalance = CLIENT.balance;
        vm.prank(CLIENT);
        escrow.refund();
        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Refunded), "refunded");
        require(CLIENT.balance == beforeBalance + DEPOSIT, "refund");
    }

    function testFuzzRefundEmptiesEscrow(uint96 rawAmount) public {
        uint256 amount = uint256(rawAmount);
        if (amount == 0) {
            return;
        }

        FreelanceEscrow escrow = _deployWithAmount(amount);
        vm.prank(CLIENT);
        escrow.refund();

        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Refunded), "refunded");
        require(address(escrow).balance == 0, "escrow empty");
    }

    function testOnlyFreelancerCanMarkDelivered() public {
        FreelanceEscrow escrow = _deploy();
        vm.expectRevert(FreelanceEscrow.OnlyFreelancer.selector);
        vm.prank(STRANGER);
        escrow.markDelivered();
    }

    function testFuzzOnlyFreelancerCanMarkDelivered(address caller) public {
        if (caller == FREELANCER) {
            return;
        }

        FreelanceEscrow escrow = _deploy();
        vm.expectRevert(FreelanceEscrow.OnlyFreelancer.selector);
        vm.prank(caller);
        escrow.markDelivered();

        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Funded), "state unchanged");
    }

    function testOnlyClientCanRelease() public {
        FreelanceEscrow escrow = _deploy();
        vm.prank(FREELANCER);
        escrow.markDelivered();
        vm.expectRevert(FreelanceEscrow.OnlyClient.selector);
        vm.prank(STRANGER);
        escrow.release();
    }

    function testFuzzOnlyClientCanRelease(address caller) public {
        if (caller == CLIENT) {
            return;
        }

        FreelanceEscrow escrow = _deploy();
        vm.prank(FREELANCER);
        escrow.markDelivered();

        vm.expectRevert(FreelanceEscrow.OnlyClient.selector);
        vm.prank(caller);
        escrow.release();

        require(uint256(escrow.state()) == uint256(FreelanceEscrow.State.Delivered), "state unchanged");
        require(address(escrow).balance == DEPOSIT, "funds retained");
    }

    function testCannotRefundAfterDelivery() public {
        FreelanceEscrow escrow = _deploy();
        vm.prank(FREELANCER);
        escrow.markDelivered();
        vm.expectRevert(
            abi.encodeWithSelector(
                FreelanceEscrow.InvalidState.selector,
                FreelanceEscrow.State.Funded,
                FreelanceEscrow.State.Delivered
            )
        );
        vm.prank(CLIENT);
        escrow.refund();
    }
}
