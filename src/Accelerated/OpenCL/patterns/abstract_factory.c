#include "abstract_factory.h"
#include "opencl_harness.h"

/* A factory-selected button and checkbox become paired device-side resources. */
int run_abstract_factory(const char *kernel_path) {
    const int a = 1;
    const int b = 1;
    return gk_run_kernel(kernel_path, a, b);
}
