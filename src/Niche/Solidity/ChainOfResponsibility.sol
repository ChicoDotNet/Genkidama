// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract ChainOfResponsibilityExample {
    enum Handler { Faq, Billing, Escalation }

    function canHandle(Handler handler, uint256 amount) public pure returns (bool) {
        if (handler == Handler.Faq) return amount <= 50;
        if (handler == Handler.Billing) return amount <= 500;
        return true;
    }

    function route(uint256 amount)
        public
        pure
        returns (string memory visited, string memory handled, string memory result)
    {
        if (canHandle(Handler.Faq, amount)) {
            return ("faq", "faq", "refund");
        }
        if (canHandle(Handler.Billing, amount)) {
            return ("faq>billing", "billing", "refund");
        }
        return ("faq>billing>escalation", "escalation", "refund");
    }
}
