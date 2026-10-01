#include "flyweight.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_flyweight(argv[1]) == 1);
    return 0;
}
