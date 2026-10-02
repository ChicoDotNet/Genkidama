#include "active_object.h"
#include "opencl_harness.h"

/* Queued work executes asynchronously from the host-facing request boundary. */
int run_active_object(const char *kernel_path) {
    const int a = 3;
    const int b = 0;
    return gk_run_kernel(kernel_path, a, b);
}
