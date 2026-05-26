#include <cuda.h>
#include <stdio.h>
#include <stdlib.h>

static void check(CUresult r, const char *msg) {
    if (r != CUDA_SUCCESS) {
        const char *s;
        cuGetErrorString(r, &s);
        fprintf(stderr, "CUDA error [%s]: %s\n", msg, s);
        exit(1);
    }
}

static char *read_ptx(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); exit(1); }
    fseek(f, 0, SEEK_END);
    long sz = ftell(f);
    rewind(f);
    char *buf = (char *)malloc(sz + 1);
    fread(buf, 1, sz, f);
    buf[sz] = '\0';
    fclose(f);
    return buf;
}

int main(int argc, char **argv) {

    const char *ptx_path = (argc > 1) ? argv[1] : "CudaCall.thrust.ptx";

    CUresult r;
    r = cuInit(0);                          check(r, "cuInit");

    CUdevice dev;
    r = cuDeviceGet(&dev, 0);               check(r, "cuDeviceGet");

    // Nombre de la GPU
    char gpu_name[128];
    cuDeviceGetName(gpu_name, sizeof(gpu_name), dev);
    printf("\n===========================================\n");
    printf("  GPU: %s\n", gpu_name);
    printf("===========================================\n\n");

    CUcontext ctx;
    r = cuCtxCreate_v4(&ctx, NULL, 0, dev); check(r, "cuCtxCreate");

    char *ptx_src = read_ptx(ptx_path);
    CUmodule mod;
    r = cuModuleLoadData(&mod, ptx_src);    check(r, "cuModuleLoadData");
    free(ptx_src);

    CUfunction fn_vecAdd;
    r = cuModuleGetFunction(&fn_vecAdd, mod, "vecAdd");
    check(r, "cuModuleGetFunction vecAdd");

    CUfunction fn_vector_sum;
    r = cuModuleGetFunction(&fn_vector_sum, mod, "vectorSum");
    check(r, "cuModuleGetFunction vectorSum");

    printf("[ vecAdd ]  c[i] = a[i] + b[i]\n");
    printf("-------------------------------------------\n");

    const int N = 1024;
    size_t bytes = N * sizeof(float);

    float *h_a = (float *)malloc(bytes);
    float *h_b = (float *)malloc(bytes);
    float *h_c = (float *)malloc(bytes);

    for (int i = 0; i < N; i++) {
        h_a[i] = (float)i;
        h_b[i] = (float)(N - i);
    }

    CUdeviceptr d_a, d_b, d_c;
    
    r = cuMemAlloc(&d_a, bytes); check(r, "cuMemAlloc d_a");
    r = cuMemAlloc(&d_b, bytes); check(r, "cuMemAlloc d_b");
    r = cuMemAlloc(&d_c, bytes); check(r, "cuMemAlloc d_c");
    r = cuMemcpyHtoD(d_a, h_a, bytes); check(r, "HtoD a");
    r = cuMemcpyHtoD(d_b, h_b, bytes); check(r, "HtoD b");

    int n_s32 = N;
    void *args_vecAdd[] = { &d_a, &d_b, &d_c, &n_s32 };

    r = cuLaunchKernel(fn_vecAdd, 4, 1, 1, 256, 1, 1, 0, NULL, args_vecAdd, NULL);
    check(r, "launch vecAdd");
    r = cuCtxSynchronize(); check(r, "sync vecAdd");
    r = cuMemcpyDtoH(h_c, d_c, bytes); check(r, "DtoH c");

    printf("  %6s  %8s  %8s  %8s\n", "idx", "a[i]", "b[i]", "c[i]");
    printf("  %6s  %8s  %8s  %8s\n", "------", "--------", "--------", "--------");

    int sample[] = {0, 1, 2, 100, 511, 512, 1021, 1022, 1023};
    int ns = sizeof(sample) / sizeof(sample[0]);
    int ok = 1;
    
    for (int i = 0; i < ns; i++) {
        int idx = sample[i];
        printf("  %6d  %8.1f  %8.1f  %8.1f\n", idx, h_a[idx], h_b[idx], h_c[idx]);
    }

    for (int i = 0; i < N; i++) {
        if (h_c[i] != h_a[i] + h_b[i]) { ok = 0; printf("  FAIL at %d\n", i); break; }
    }

    printf("  Result: %s  (%d verified elements)\n\n", ok ? "OK ✓" : "FAIL ✗", N);

    printf("[ vectorSum ]  block reduction\n");
    printf("-------------------------------------------\n");

    const int THREADS = 256;
    const int BLOCKS  = (N + THREADS - 1) / THREADS;

    for (int i = 0; i < N; i++) h_a[i] = (float)(i + 1);

    r = cuMemcpyHtoD(d_a, h_a, bytes); check(r, "HtoD input");

    CUdeviceptr d_out;
    r = cuMemAlloc(&d_out, BLOCKS * sizeof(float)); check(r, "cuMemAlloc d_out");
    r = cuMemsetD32(d_out, 0, BLOCKS);              check(r, "cuMemsetD32");

    unsigned int n_u32 = (unsigned int)N;
    void *args_vsum[] = { &d_a, &d_out, &n_u32 };

    r = cuLaunchKernel(fn_vector_sum, BLOCKS, 1, 1, THREADS, 1, 1, 0, NULL, args_vsum, NULL);
    check(r, "launch vectorSum");
    r = cuCtxSynchronize(); check(r, "sync vectorSum");

    float h_out[4] = {0};
    r = cuMemcpyDtoH(h_out, d_out, BLOCKS * sizeof(float)); check(r, "DtoH out");

    float expected_total = (float)(N * (N + 1) / 2);
    float total = 0.0f;
    
    printf("  %8s  %12s  %12s\n", "block", "parcial GPU", "parcial CPU");
    printf("  %8s  %12s  %12s\n", "--------", "------------", "------------");
    
    for (int b = 0; b < BLOCKS; b++) {
        float cpu_partial = 0.0f;
        
        for (int i = b * THREADS; i < (b + 1) * THREADS && i < N; i++)
            cpu_partial += h_a[i];

        printf("  %8d  %12.1f  %12.1f  %s\n",
               b, h_out[b], cpu_partial,
               h_out[b] == cpu_partial ? "OK ✓" : "FAIL ✗"
        );
        
        total += h_out[b];
    }

    printf("  Total GPU: %.1f  |  Expected: %.1f  |  %s\n\n",
           total, expected_total,
           total == expected_total ? "OK ✓" : "FAIL ✗"
    );

    printf("===========================================\n\n");

    cuMemFree(d_a); 
    cuMemFree(d_b); 
    cuMemFree(d_c); 
    cuMemFree(d_out);
    
    free(h_a); 
    free(h_b); 
    free(h_c);
    
    cuModuleUnload(mod);
    cuCtxDestroy(ctx);

    return 0;
}
