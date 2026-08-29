#include <string>

bool run()
{
    auto mediate = [](const std::string& message) -> std::string {
        return message == "ping" ? "pong" : "unknown";
    };

    return mediate("ping") == "pong";
}
