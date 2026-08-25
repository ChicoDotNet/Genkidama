#include <stdio.h>

static void authenticate(char *out, size_t size, const char *user) {
    snprintf(out, size, "auth(%s)", user);
}

static void reserve(char *out, size_t size, const char *sku) {
    snprintf(out, size, "reserve(%s)", sku);
}

static void charge(char *out, size_t size, int cents) {
    snprintf(out, size, "charge(%d)", cents);
}

static void checkout(char *out, size_t size, const char *user, const char *sku, int cents) {
    char auth[64];
    char inventory[64];
    char billing[64];
    authenticate(auth, sizeof auth, user);
    reserve(inventory, sizeof inventory, sku);
    charge(billing, sizeof billing, cents);
    snprintf(out, size, "checkout=%s>%s>%s", auth, inventory, billing);
}

int main(void) {
    char result[256];
    checkout(result, sizeof result, "alice", "SKU-42", 499);
    puts(result);
    return 0;
}
