/* A builder assembles the two-dimensional work configuration consumed by the kernel. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a * b;
}
