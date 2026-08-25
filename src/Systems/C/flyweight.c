#include <stdio.h>
#include <string.h>

typedef struct {
    const char *font;
    int size;
    const char *color;
} TextStyle;

typedef struct {
    TextStyle styles[8];
    int count;
} StyleFactory;

static TextStyle *get_style(StyleFactory *factory, const char *font, int size, const char *color) {
    for (int i = 0; i < factory->count; ++i) {
        TextStyle *style = &factory->styles[i];
        if (style->size == size && strcmp(style->font, font) == 0 && strcmp(style->color, color) == 0) {
            return style;
        }
    }

    TextStyle *style = &factory->styles[factory->count++];
    style->font = font;
    style->size = size;
    style->color = color;
    return style;
}

int main(void) {
    StyleFactory factory = {0};
    TextStyle *red1 = get_style(&factory, "Inter", 12, "red");
    TextStyle *red2 = get_style(&factory, "Inter", 12, "red");
    (void)get_style(&factory, "Inter", 12, "blue");
    printf("styles=%d;shared=%s;text=ABC\n", factory.count, red1 == red2 ? "true" : "false");
    return 0;
}
