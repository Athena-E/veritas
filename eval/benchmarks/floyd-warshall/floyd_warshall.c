#include <stdio.h>
#include <time.h>

#ifndef N
#define N 1024
#endif

static long long path[N][N];

static void init_array(void) {
    for (int i = 0; i < N; i++)
        for (int j = 0; j < N; j++)
            path[i][j] = ((long long)(i + 1) * (j + 1)) / N;
}

static void kernel(void) {
    for (int k = 0; k < N; k++)
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++) {
                long long via = path[i][k] + path[k][j];
                if (via < path[i][j]) path[i][j] = via;
            }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++)
        for (int j = 0; j < N; j++)
            s += path[i][j];
    return s;
}

int main(void) {
    init_array();

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    kernel();
    clock_gettime(CLOCK_MONOTONIC, &t1);

    double elapsed = (t1.tv_sec - t0.tv_sec) + (t1.tv_nsec - t0.tv_nsec) / 1e9;
    fprintf(stderr, "checksum=%lld\n", checksum());
    printf("%.6f\n", elapsed);
    return 0;
}
