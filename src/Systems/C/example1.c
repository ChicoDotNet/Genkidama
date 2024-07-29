#include <stdio.h>

// Abstract Factory
typedef struct {
    void (*create_button)();
    void (*create_checkbox)();
} UIFactory;

// Concrete Products
void dark_button() {
    printf("Dark Button\n");
}

void light_button() {
    printf("Light Button\n");
}

void dark_checkbox() {
    printf("Dark Checkbox\n");
}

void light_checkbox() {
    printf("Light Checkbox\n");
}

// Concrete Factories
void create_dark_button() {
    dark_button();
}

void create_light_button() {
    light_button();
}

void create_dark_checkbox() {
    dark_checkbox();
}

void create_light_checkbox() {
    light_checkbox();
}

UIFactory dark_factory = {create_dark_button, create_dark_checkbox};
UIFactory light_factory = {create_light_button, create_light_checkbox};

// Usage
void create_ui_components(UIFactory factory) {
    factory.create_button();
    factory.create_checkbox();
}

int main() {
    create_ui_components(dark_factory);
    create_ui_components(light_factory);
    return 0;
}
