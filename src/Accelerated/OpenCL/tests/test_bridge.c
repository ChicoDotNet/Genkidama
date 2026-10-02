#include "bridge.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_bridge(argv[1]) == 2);
    return 0;
}
