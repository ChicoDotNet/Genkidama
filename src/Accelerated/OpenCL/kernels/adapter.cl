/* The kernel adapts legacy Fahrenheit input to the Celsius contract. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = (a - 32) * 5 / 9;
}
