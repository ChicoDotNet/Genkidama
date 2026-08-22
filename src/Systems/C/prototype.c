#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    char name[64];
    char features[4][32];
    size_t feature_count;
} ServiceProfile;

static ServiceProfile clone_profile(const ServiceProfile *source) {
    ServiceProfile clone = *source;
    return clone;
}

static void describe(const char *label, const ServiceProfile *profile) {
    printf("%s=%s: ", label, profile->name);
    for (size_t i = 0; i < profile->feature_count; ++i) {
        if (i > 0) {
            putchar(',');
        }
        fputs(profile->features[i], stdout);
    }
    putchar('\n');
}

int main(void) {
    ServiceProfile original = {
        .name = "orders",
        .features = {"metrics"},
        .feature_count = 1,
    };

    ServiceProfile clone = clone_profile(&original);
    strcpy(clone.name, "orders-canary");
    strcpy(clone.features[clone.feature_count++], "tracing");

    describe("original", &original);
    describe("clone", &clone);
    return EXIT_SUCCESS;
}
