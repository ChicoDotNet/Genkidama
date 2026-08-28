// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library ClientServerPattern {
    struct Response { uint256 status; uint256 stock; }
    function server(uint256 sku) private pure returns (Response memory) {
        return sku == 1 ? Response(200, 7) : Response(404, 0);
    }
    function run() internal pure returns (bool) {
        Response memory response = server(1);
        return response.status == 200 && response.stock == 7;
    }
}
