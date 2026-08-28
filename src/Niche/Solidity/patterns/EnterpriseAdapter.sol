// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library EnterpriseAdapterPattern {
    struct Legacy { uint256 code; uint256 cents; }
    struct Canonical { uint256 id; uint256 whole; uint256 cents; }
    function adapt(Legacy memory legacy) private pure returns (Canonical memory) {
        return Canonical(legacy.code, legacy.cents / 100, legacy.cents % 100);
    }
    function run() internal pure returns (bool) {
        Canonical memory canonical = adapt(Legacy(17, 1250));
        return canonical.id == 17 && canonical.whole == 12 && canonical.cents == 50;
    }
}
