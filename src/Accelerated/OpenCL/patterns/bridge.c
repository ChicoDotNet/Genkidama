#include "bridge.h"
#include "opencl_harness.h"

/* Host abstraction and device implementation vary independently before dispatch. */
int run_bridge(const char *kernel_path) {
    const int a = 1;
    const int b = 1;
    return gk_run_kernel(kernel_path, a, b);
}
