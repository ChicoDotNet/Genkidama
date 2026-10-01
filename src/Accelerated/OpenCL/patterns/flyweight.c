#include "flyweight.h"
#include "opencl_harness.h"

/* Equivalent kernel/program keys reuse shared intrinsic state. */
int run_flyweight(const char *kernel_path) {
    const int a = 7;
    const int b = 7;
    return gk_run_kernel(kernel_path, a, b);
}
