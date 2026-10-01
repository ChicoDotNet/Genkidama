/* Queued work executes asynchronously from the host-facing request boundary. */
__kernel void pattern_kernel(const int a, const int b, __global int *out) {
    out[0] = a * a;
}
