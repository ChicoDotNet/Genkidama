/* The host command encapsulates an enqueueable operation and its arguments. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a + b;
}
