/* A facade hides context/program/kernel orchestration behind one operation. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a + b;
}
