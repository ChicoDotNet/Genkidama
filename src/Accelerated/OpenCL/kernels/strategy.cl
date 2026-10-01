/* The selected compute strategy squares its input on the OpenCL device. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a * a;
}
