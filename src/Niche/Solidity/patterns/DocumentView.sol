// SPDX-License-Identifier: MIT
pragma solidity >=0.8.0 <0.9.0;

library DocumentViewPattern {
    struct Document { uint256 titleId; uint256 words; }
    function editor(Document memory document) private pure returns (uint256, uint256) { return (document.titleId, document.words); }
    function summary(Document memory document) private pure returns (uint256) { return document.titleId; }
    function run() internal pure returns (bool) {
        Document memory document = Document(7, 120);
        (uint256 title, uint256 words) = editor(document);
        return title == 7 && words == 120 && summary(document) == 7;
    }
}
