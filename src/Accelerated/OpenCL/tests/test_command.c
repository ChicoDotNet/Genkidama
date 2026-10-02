#include "command.h"

#include <assert.h>

int main(int argc, char **argv) {
    assert(argc == 2);
    assert(run_command(argv[1]) == 7);
    return 0;
}
