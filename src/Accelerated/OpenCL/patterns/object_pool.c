#include "object_pool.h"
#include "opencl_harness.h"

/* A returned buffer identity is reused instead of creating a second logical resource. */
int run_object_pool(const char *kernel_path) {
    const int a = 7;
    const int b = 7;
    return gk_run_kernel(kernel_path, a, b);
}
