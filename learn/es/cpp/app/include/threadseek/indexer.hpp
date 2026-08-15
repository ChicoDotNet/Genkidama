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

/// Discovers regular files below root without owning the resulting index.
[[nodiscard]] std::vector<FileRecord> discover_files(const std::filesystem::path& root);

/// Owns an in-memory file index and deterministic search operations.
class FileIndex {
public:
    /// Recursively indexes regular files under root.
    explicit FileIndex(const std::filesystem::path& root);

    /// Reconstructs an index from already discovered or persisted records.
    explicit FileIndex(std::vector<FileRecord> records);

    [[nodiscard]] const std::vector<FileRecord>& files() const noexcept;
    [[nodiscard]] std::vector<FileRecord> search(std::string_view query) const;
    [[nodiscard]] std::uintmax_t total_size_bytes() const noexcept;

private:
    std::vector<FileRecord> files_;
};

/// Persists and reconstructs indexes using a small text format.
class IndexStore {
public:
    /// Writes the complete index through a temporary file and replaces destination.
    static void save(const FileIndex& index, const std::filesystem::path& destination);

    /// Loads an index previously written by save().
    /// Throws std::runtime_error for malformed or unreadable input.
    [[nodiscard]] static FileIndex load(const std::filesystem::path& source);
};

}  // namespace threadseek
