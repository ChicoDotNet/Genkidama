#define CL_TARGET_OPENCL_VERSION 120
#include <CL/cl.h>

#include "opencl_harness.h"

#include <stdio.h>
#include <stdlib.h>

static char *read_text(const char *path, size_t *length) {
    FILE *file = fopen(path, "rb");
    if (file == NULL) return NULL;
    if (fseek(file, 0, SEEK_END) != 0) { fclose(file); return NULL; }
    long size = ftell(file);
    if (size < 0 || fseek(file, 0, SEEK_SET) != 0) { fclose(file); return NULL; }
    char *buffer = malloc((size_t)size + 1);
    if (buffer == NULL) { fclose(file); return NULL; }
    size_t read = fread(buffer, 1, (size_t)size, file);
    fclose(file);
    if (read != (size_t)size) { free(buffer); return NULL; }
    buffer[read] = '\0';
    *length = read;
    return buffer;
}

int gk_run_kernel(const char *kernel_path, int a, int b) {
    cl_int err = CL_SUCCESS;
    cl_platform_id platform = NULL;
    cl_device_id device = NULL;
    cl_context context = NULL;
    cl_command_queue queue = NULL;
    cl_program program = NULL;
    cl_kernel kernel = NULL;
    cl_mem output = NULL;
    char *source = NULL;
    int result = -1000;
    size_t source_len = 0;

    if (clGetPlatformIDs(1, &platform, NULL) != CL_SUCCESS) goto cleanup;
    if (clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL, 1, &device, NULL) != CL_SUCCESS) goto cleanup;

    context = clCreateContext(NULL, 1, &device, NULL, NULL, &err);
    if (err != CL_SUCCESS || context == NULL) goto cleanup;

    queue = clCreateCommandQueue(context, device, 0, &err);
    if (err != CL_SUCCESS || queue == NULL) goto cleanup;

    source = read_text(kernel_path, &source_len);
    if (source == NULL) goto cleanup;

    const char *sources[] = {source};
    program = clCreateProgramWithSource(context, 1, sources, &source_len, &err);
    if (err != CL_SUCCESS || program == NULL) goto cleanup;

    err = clBuildProgram(program, 1, &device, "", NULL, NULL);
    if (err != CL_SUCCESS) {
        size_t log_size = 0;
        clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);
        if (log_size > 1) {
            char *log = malloc(log_size);
            if (log != NULL) {
                clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, log_size, log, NULL);
                fputs(log, stderr);
                free(log);
            }
        }
        goto cleanup;
    }

    kernel = clCreateKernel(program, "pattern_kernel", &err);
    if (err != CL_SUCCESS || kernel == NULL) goto cleanup;

    output = clCreateBuffer(context, CL_MEM_WRITE_ONLY, sizeof(result), NULL, &err);
    if (err != CL_SUCCESS || output == NULL) goto cleanup;

    if (clSetKernelArg(kernel, 0, sizeof(a), &a) != CL_SUCCESS) goto cleanup;
    if (clSetKernelArg(kernel, 1, sizeof(b), &b) != CL_SUCCESS) goto cleanup;
    if (clSetKernelArg(kernel, 2, sizeof(output), &output) != CL_SUCCESS) goto cleanup;

    const size_t global = 1;
    if (clEnqueueNDRangeKernel(queue, kernel, 1, NULL, &global, NULL, 0, NULL, NULL) != CL_SUCCESS) goto cleanup;
    if (clEnqueueReadBuffer(queue, output, CL_TRUE, 0, sizeof(result), &result, 0, NULL, NULL) != CL_SUCCESS) {
        result = -1000;
        goto cleanup;
    }

cleanup:
    if (output != NULL) clReleaseMemObject(output);
    if (kernel != NULL) clReleaseKernel(kernel);
    if (program != NULL) clReleaseProgram(program);
    if (queue != NULL) clReleaseCommandQueue(queue);
    if (context != NULL) clReleaseContext(context);
    free(source);
    return result;
}
