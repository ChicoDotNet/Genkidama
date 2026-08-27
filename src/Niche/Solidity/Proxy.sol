// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

interface DocumentStore {
    function get(uint256 id) external returns (bytes32);
}

contract RemoteDocumentStore is DocumentStore {
    uint256 public fetches;

    function get(uint256 id) external returns (bytes32) {
        fetches += 1;
        return keccak256(abi.encodePacked("doc(", id, ")"));
    }
}

contract DocumentStoreProxy is DocumentStore {
    RemoteDocumentStore private backend;
    mapping(uint256 => bytes32) private cache;
    mapping(uint256 => bool) private cached;

    function get(uint256 id) external returns (bytes32) {
        if (!cached[id]) {
            if (address(backend) == address(0)) {
                backend = new RemoteDocumentStore();
            }
            cache[id] = backend.get(id);
            cached[id] = true;
        }
        return cache[id];
    }

    function backendCreated() external view returns (bool) {
        return address(backend) != address(0);
    }

    function backendFetches() external view returns (uint256) {
        if (address(backend) == address(0)) {
            return 0;
        }
        return backend.fetches();
    }
}
