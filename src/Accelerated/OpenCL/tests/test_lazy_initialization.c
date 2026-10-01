#include "lazy_initialization.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_lazy_initialization(argv[1]) == 1);
    return 0;
}
