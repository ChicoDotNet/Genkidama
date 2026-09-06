#include <stdbool.h>
#include <stddef.h>
#include <string.h>

typedef bool (*receive_fn)(const char *sender, const char *message, char *events, size_t events_size);

typedef struct {
    const char *name;
    receive_fn receive;
} colleague;

typedef struct {
    colleague colleagues[2];
    size_t count;
} checkout_mediator;

static bool append_event(char *events, size_t events_size, const char *event) {
    size_t used = strlen(events);
    size_t needed = strlen(event);
    if (used + needed + 1 > events_size) {
        return false;
    }
    memcpy(events + used, event, needed + 1);
    return true;
}

static bool inventory_receive(const char *sender, const char *message, char *events, size_t events_size) {
    return strcmp(sender, "payment") == 0 && strcmp(message, "paid") == 0 &&
           append_event(events, events_size, "inventory<-payment:paid");
}

static bool payment_receive(const char *sender, const char *message, char *events, size_t events_size) {
    return strcmp(sender, "inventory") == 0 && strcmp(message, "reserved") == 0 &&
           append_event(events, events_size, ">payment<-inventory:reserved");
}

static void mediator_register(checkout_mediator *mediator, const char *name, receive_fn receive) {
    mediator->colleagues[mediator->count++] = (colleague){name, receive};
}

static bool mediator_send(
    const checkout_mediator *mediator,
    const char *sender,
    const char *recipient,
    const char *message,
    char *events,
    size_t events_size) {
    for (size_t i = 0; i < mediator->count; ++i) {
        if (strcmp(mediator->colleagues[i].name, recipient) == 0) {
            return mediator->colleagues[i].receive(sender, message, events, events_size);
        }
    }
    return false;
}

bool run(void) {
    checkout_mediator mediator = {0};
    char events[96] = "";

    mediator_register(&mediator, "inventory", inventory_receive);
    mediator_register(&mediator, "payment", payment_receive);

    if (!mediator_send(&mediator, "payment", "inventory", "paid", events, sizeof events)) {
        return false;
    }
    if (!mediator_send(&mediator, "inventory", "payment", "reserved", events, sizeof events)) {
        return false;
    }
    if (mediator_send(&mediator, "payment", "unknown", "ignored", events, sizeof events)) {
        return false;
    }

    return strcmp(events, "inventory<-payment:paid>payment<-inventory:reserved") == 0;
}
