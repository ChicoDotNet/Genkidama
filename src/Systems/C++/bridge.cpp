#include <iostream>
#include <memory>
#include <string>

struct Device {
    virtual ~Device() = default;
    virtual std::string power_on() const = 0;
    virtual std::string mute() const = 0;
};

struct TvDevice final : Device {
    std::string power_on() const override { return "TV:on"; }
    std::string mute() const override { return "TV:muted"; }
};

struct RadioDevice final : Device {
    std::string power_on() const override { return "Radio:on"; }
    std::string mute() const override { return "Radio:muted"; }
};

class RemoteControl {
public:
    explicit RemoteControl(const Device& device) : device_(device) {}
    virtual ~RemoteControl() = default;
    virtual std::string activate() const = 0;
protected:
    const Device& device_;
};

class BasicRemote final : public RemoteControl {
public:
    using RemoteControl::RemoteControl;
    std::string activate() const override { return device_.power_on(); }
};

class MuteRemote final : public RemoteControl {
public:
    using RemoteControl::RemoteControl;
    std::string activate() const override { return device_.mute(); }
};

int main() {
    TvDevice tv;
    RadioDevice radio;
    std::cout << "basic-tv=" << BasicRemote(tv).activate() << '\n';
    std::cout << "basic-radio=" << BasicRemote(radio).activate() << '\n';
    std::cout << "mute-tv=" << MuteRemote(tv).activate() << '\n';
    std::cout << "mute-radio=" << MuteRemote(radio).activate() << '\n';
}
