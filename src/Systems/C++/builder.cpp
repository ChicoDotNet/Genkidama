#include <iostream>
#include <string>
#include <vector>

class ReportBuilder {
public:
    virtual ~ReportBuilder() = default;
    virtual void reset() = 0;
    virtual void addTitle(const std::string& title) = 0;
    virtual void addSection(const std::string& heading, const std::string& body) = 0;
    virtual std::string build() const = 0;
};

class TextReportBuilder final : public ReportBuilder {
public:
    void reset() override { parts_.clear(); }
    void addTitle(const std::string& title) override { parts_.push_back("# " + title); }
    void addSection(const std::string& heading, const std::string& body) override {
        parts_.push_back("## " + heading);
        parts_.push_back(body);
    }
    std::string build() const override {
        std::string result;
        for (std::size_t i = 0; i < parts_.size(); ++i) {
            if (i != 0) result += '\n';
            result += parts_[i];
        }
        return result;
    }
private:
    std::vector<std::string> parts_;
};

class HtmlReportBuilder final : public ReportBuilder {
public:
    void reset() override { parts_.clear(); }
    void addTitle(const std::string& title) override { parts_.push_back("<h1>" + title + "</h1>"); }
    void addSection(const std::string& heading, const std::string& body) override {
        parts_.push_back("<h2>" + heading + "</h2>");
        parts_.push_back("<p>" + body + "</p>");
    }
    std::string build() const override {
        std::string result;
        for (const auto& part : parts_) result += part;
        return result;
    }
private:
    std::vector<std::string> parts_;
};

std::string buildAvailabilityReport(ReportBuilder& builder) {
    builder.reset();
    builder.addTitle("Service status");
    builder.addSection("Availability", "99.95%");
    return builder.build();
}

int main() {
    TextReportBuilder text;
    HtmlReportBuilder html;
    std::cout << buildAvailabilityReport(text) << "\n---\n" << buildAvailabilityReport(html) << '\n';
}
