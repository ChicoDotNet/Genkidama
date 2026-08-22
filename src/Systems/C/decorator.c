#include <stdio.h>
#include <string.h>

typedef struct Component Component;
typedef void (*RenderFn)(const Component *, char *, size_t);

struct Component {
    RenderFn render;
    const Component *inner;
};

static void render_plain(const Component *self, char *out, size_t out_size) {
    (void)self;
    snprintf(out, out_size, "alert");
}

static void render_audit(const Component *self, char *out, size_t out_size) {
    char inner[128];
    self->inner->render(self->inner, inner, sizeof inner);
    snprintf(out, out_size, "audit(%s)", inner);
}

static void render_encrypt(const Component *self, char *out, size_t out_size) {
    char inner[128];
    self->inner->render(self->inner, inner, sizeof inner);
    snprintf(out, out_size, "enc(%s)", inner);
}

static void print_rendered(const char *label, const Component *component) {
    char output[128];
    component->render(component, output, sizeof output);
    printf("%s=%s\n", label, output);
}

int main(void) {
    const Component plain = { render_plain, NULL };
    const Component audit = { render_audit, &plain };
    const Component encrypted = { render_encrypt, &plain };
    const Component stacked_encrypted = { render_encrypt, &plain };
    const Component stacked = { render_audit, &stacked_encrypted };

    print_rendered("base", &plain);
    print_rendered("audit", &audit);
    print_rendered("encrypted", &encrypted);
    print_rendered("stacked", &stacked);
    return 0;
}
