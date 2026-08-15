#include "threadseek/indexer.hpp"

#include <algorithm>
#include <cctype>
#include <stdexcept>

namespace threadseek {
namespace {

std::string ascii_lower(std::string text) {
    std::transform(text.begin(), text.end(), text.begin(), [](const unsigned char ch) {
        return static_cast<char>(std::tolower(ch));
    });
    return text;
}

}  // namespace

FileIndex::FileIndex(const std::filesystem::path& root) {
    std::error_code error;
    if (!std::filesystem::exists(root, error) || error) {
        throw std::invalid_argument("La ruta raíz no existe: " + root.string());
    }
    if (!std::filesystem::is_directory(root, error) || error) {
        throw std::invalid_argument("La ruta raíz no es un directorio: " + root.string());
    }

    const auto options = std::filesystem::directory_options::skip_permission_denied;
    std::filesystem::recursive_directory_iterator iterator(root, options, error);
    const std::filesystem::recursive_directory_iterator end;

    while (iterator != end) {
        if (error) {
            error.clear();
            iterator.increment(error);
            continue;
        }

        const auto& entry = *iterator;
        if (entry.is_regular_file(error) && !error) {
            const auto size = entry.file_size(error);
            if (!error) {
                files_.push_back(FileRecord{entry.path(), size});
            }
        }
        error.clear();
        iterator.increment(error);
    }

    std::sort(files_.begin(), files_.end(), [](const FileRecord& left, const FileRecord& right) {
        return left.path.generic_string() < right.path.generic_string();
    });
}

const std::vector<FileRecord>& FileIndex::files() const noexcept {
    return files_;
}

std::vector<FileRecord> FileIndex::search(const std::string_view query) const {
    const auto normalized_query = ascii_lower(std::string(query));
    std::vector<FileRecord> matches;

    for (const auto& record : files_) {
        const auto filename = ascii_lower(record.path.filename().string());
        if (filename.contains(normalized_query)) {
            matches.push_back(record);
        }
    }

    return matches;
}

std::uintmax_t FileIndex::total_size_bytes() const noexcept {
    std::uintmax_t total = 0;
    for (const auto& record : files_) {
        total += record.size_bytes;
    }
    return total;
}

}  // namespace threadseek
