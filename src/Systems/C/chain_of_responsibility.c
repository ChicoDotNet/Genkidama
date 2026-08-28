#include <stdio.h>
#include <string.h>

typedef struct Handler Handler;

struct Handler {
    const char *name;
    int limit;
    Handler *next;
};

static const char *handle(Handler *handler, int amount, char *visited, size_t capacity) {
    size_t used = strlen(visited);
    (void)snprintf(visited + used, capacity - used, "%s%s", used == 0U ? "" : ">", handler->name);

    if (amount <= handler->limit || handler->next == NULL) {
        return handler->name;
    }

    return handle(handler->next, amount, visited, capacity);
}

int main(void) {
    Handler escalation = {"escalation", 2147483647, NULL};
    Handler billing = {"billing", 500, &escalation};
    Handler faq = {"faq", 50, &billing};
    char visited[64] = "";

    const char *handled = handle(&faq, 250, visited, sizeof visited);
    printf("visited=%s;handled=%s;result=refund(250)\n", visited, handled);
    return 0;
}
