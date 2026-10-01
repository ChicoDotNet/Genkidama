#include "command.h"
#include "opencl_harness.h"

/* The host command encapsulates an enqueueable operation and its arguments. */
int run_command(const char *kernel_path) {
    const int a = 3;
    const int b = 4;
    return gk_run_kernel(kernel_path, a, b);
}
