#include "proxy.h"
#include "opencl_harness.h"

/* A proxy preserves the result contract while controlling device access. */
int run_proxy(const char *kernel_path) {
    const int a = 1;
    const int b = 1;
    return gk_run_kernel(kernel_path, a, b);
}
