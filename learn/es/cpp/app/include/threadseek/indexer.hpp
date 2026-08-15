#pragma once

#include <cstdint>
#include <filesystem>
#include <string>
#include <string_view>
#include <vector>

namespace threadseek {

/// Describes one regular file discovered by the indexer.
struct FileRecord {
    std::filesystem::path path;
    std::uintmax_t size_bytes{};
};

/// Owns an in-memory file index and deterministic search operations.
class FileIndex {
public:
    /// Recursively indexes regular files under root.
    /// \param root Existing directory to scan.
    /// \throws std::invalid_argument when root does not exist or is not a directory.
    explicit FileIndex(const std::filesystem::path& root);

    /// Returns indexed files ordered lexicographically by generic path text.
    [[nodiscard]] const std::vector<FileRecord>& files() const noexcept;

    /// Returns records whose filename contains query, ignoring ASCII case.
    /// Empty query matches every indexed file.
    [[nodiscard]] std::vector<FileRecord> search(std::string_view query) const;

    /// Returns the total size represented by indexed regular files.
    [[nodiscard]] std::uintmax_t total_size_bytes() const noexcept;

private:
    std::vector<FileRecord> files_;
};

}  // namespace threadseek
