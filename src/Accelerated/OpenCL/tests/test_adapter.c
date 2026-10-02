#include "adapter.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_adapter(argv[1]) == 100);
    return 0;
}
