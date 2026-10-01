#include "builder.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_builder(argv[1]) == 64);
    return 0;
}
