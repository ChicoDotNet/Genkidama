#include "builder.h"
#include "opencl_harness.h"

/* A builder assembles the two-dimensional work configuration consumed by the kernel. */
int run_builder(const char *kernel_path) {
    const int a = 8;
    const int b = 8;
    return gk_run_kernel(kernel_path, a, b);
}
