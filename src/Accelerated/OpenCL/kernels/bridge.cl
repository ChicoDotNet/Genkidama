/* Host abstraction and device implementation vary independently before dispatch. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a + b;
}
