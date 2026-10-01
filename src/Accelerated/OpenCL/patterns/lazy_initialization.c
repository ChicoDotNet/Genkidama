#include "lazy_initialization.h"
#include "opencl_harness.h"

/* Program/kernel materialization is represented as an on-demand device operation. */
int run_lazy_initialization(const char *kernel_path) {
    const int a = 0;
    const int b = 1;
    return gk_run_kernel(kernel_path, a, b);
}
