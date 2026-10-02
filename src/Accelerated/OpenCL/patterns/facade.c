#include "facade.h"
#include "opencl_harness.h"

/* A facade hides context/program/kernel orchestration behind one operation. */
int run_facade(const char *kernel_path) {
    const int a = 40;
    const int b = 2;
    return gk_run_kernel(kernel_path, a, b);
}
