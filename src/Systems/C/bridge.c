#include <stdio.h>

typedef struct Device {
    const char *name;
    const char *(*turn_on)(const struct Device *self);
    const char *(*mute)(const struct Device *self);
} Device;

static const char *device_on(const Device *self) {
    static char buffer[32];
    snprintf(buffer, sizeof buffer, "%s:on", self->name);
    return buffer;
}

static const char *device_mute(const Device *self) {
    static char buffer[32];
    snprintf(buffer, sizeof buffer, "%s:muted", self->name);
    return buffer;
}

typedef const char *(*RemoteAction)(const Device *device);

static const char *basic_remote(const Device *device) { return device->turn_on(device); }
static const char *mute_remote(const Device *device) { return device->mute(device); }

int main(void) {
    const Device tv = {"TV", device_on, device_mute};
    const Device radio = {"Radio", device_on, device_mute};
    const RemoteAction basic = basic_remote;
    const RemoteAction mute = mute_remote;

    printf("basic-tv=%s\n", basic(&tv));
    printf("basic-radio=%s\n", basic(&radio));
    printf("mute-tv=%s\n", mute(&tv));
    printf("mute-radio=%s\n", mute(&radio));
    return 0;
}
