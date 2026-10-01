#include "active_object.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_active_object(argv[1]) == 9);
    return 0;
}
