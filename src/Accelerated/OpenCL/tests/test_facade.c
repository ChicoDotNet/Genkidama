#include "facade.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_facade(argv[1]) == 42);
    return 0;
}
