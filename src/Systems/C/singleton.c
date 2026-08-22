#include <stdbool.h>
#include <stdio.h>

typedef struct {
    int count;
} Registry;

static Registry registry = {0};

static Registry *registry_instance(void) {
    return &registry;
}

int main(void) {
    Registry *first = registry_instance();
    Registry *second = registry_instance();
    first->count += 1;

    printf("same=%s\n", first == second ? "true" : "false");
    printf("count=%d\n", second->count);
    return 0;
}
