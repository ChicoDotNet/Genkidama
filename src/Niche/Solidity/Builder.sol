// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

interface IReportBuilder {
    function reset() external;
    function addTitle(string calldata title) external;
    function addSection(string calldata heading, string calldata body) external;
    function build() external view returns (string memory);
}

contract TextReportBuilder is IReportBuilder {
    string private report;

    function reset() external override { report = ""; }
    function addTitle(string calldata title) external override {
        report = string.concat(report, "# ", title, "\n");
    }
    function addSection(string calldata heading, string calldata body) external override {
        report = string.concat(report, "## ", heading, "\n", body);
    }
    function build() external view override returns (string memory) { return report; }
}

contract HtmlReportBuilder is IReportBuilder {
    string private report;

    function reset() external override { report = ""; }
    function addTitle(string calldata title) external override {
        report = string.concat(report, "<h1>", title, "</h1>");
    }
    function addSection(string calldata heading, string calldata body) external override {
        report = string.concat(report, "<h2>", heading, "</h2><p>", body, "</p>");
    }
    function build() external view override returns (string memory) { return report; }
}

contract Builder {
    function buildAvailabilityReport(IReportBuilder builder) public returns (string memory) {
        builder.reset();
        builder.addTitle("Service status");
        builder.addSection("Availability", "99.95%");
        return builder.build();
    }

    function test() external returns (string memory textReport, string memory htmlReport) {
        textReport = buildAvailabilityReport(new TextReportBuilder());
        htmlReport = buildAvailabilityReport(new HtmlReportBuilder());
    }
}
