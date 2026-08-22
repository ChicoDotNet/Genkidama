// SPDX-License-Identifier: MIT
pragma solidity ^0.8.30;

contract CompositeExample {
    struct Node {
        bool isFile;
        uint256 bytesValue;
        uint256[] children;
    }

    Node[] private nodes;

    constructor() {
        nodes.push();
        nodes[0].isFile = true;
        nodes[0].bytesValue = 2;

        nodes.push();
        nodes[1].isFile = true;
        nodes[1].bytesValue = 3;

        nodes.push();
        nodes[2].isFile = true;
        nodes[2].bytesValue = 5;

        nodes.push();
        nodes[3].children.push(1);
        nodes[3].children.push(2);

        nodes.push();
        nodes[4].children.push(0);
        nodes[4].children.push(3);
    }

    function size(uint256 nodeId) public view returns (uint256 total) {
        Node storage node = nodes[nodeId];
        if (node.isFile) {
            return node.bytesValue;
        }

        for (uint256 i = 0; i < node.children.length; ++i) {
            total += size(node.children[i]);
        }
    }

    function scenario() external view returns (uint256 leaf, uint256 docs, uint256 root) {
        return (size(0), size(3), size(4));
    }
}
