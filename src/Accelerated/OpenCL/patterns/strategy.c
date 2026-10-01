#include "strategy.h"
#include "opencl_harness.h"

/* The selected compute strategy squares its input on the OpenCL device. */
int run_strategy(const char *kernel_path) {
    const int a = 4;
    const int b = 0;
    return gk_run_kernel(kernel_path, a, b);
}
