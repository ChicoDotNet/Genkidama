/* Program/kernel materialization is represented as an on-demand device operation. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = b == 1 ? 1 : 0;
}
