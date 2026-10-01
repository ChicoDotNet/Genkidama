/* A proxy preserves the result contract while controlling device access. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a == b ? 1 : 0;
}
