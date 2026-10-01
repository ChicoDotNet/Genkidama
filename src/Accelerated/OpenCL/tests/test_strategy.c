#include "strategy.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_strategy(argv[1]) == 16);
    return 0;
}
