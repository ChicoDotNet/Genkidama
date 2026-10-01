#include "abstract_factory.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_abstract_factory(argv[1]) == 2);
    return 0;
}
