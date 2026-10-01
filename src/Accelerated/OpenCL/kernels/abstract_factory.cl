/* A factory-selected button and checkbox become paired device-side resources. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a + b;
}
