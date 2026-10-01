#include "adapter.h"
#include "opencl_harness.h"

/* The kernel adapts legacy Fahrenheit input to the Celsius contract. */
int run_adapter(const char *kernel_path) {
    const int a = 212;
    const int b = 0;
    return gk_run_kernel(kernel_path, a, b);
}
