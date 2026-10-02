/* A returned buffer identity is reused instead of creating a second logical resource. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a == b ? 1 : 0;
}
