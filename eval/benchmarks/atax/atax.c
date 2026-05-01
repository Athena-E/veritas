#include <stdio.h>

#ifndef N
#define N 96
#endif

static long long a[N][N];
static long long x[N];
static long long y[N];
static long long tmp[N];

static void init_array(void) {
    for (int i = 0; i < N; i++) {
        x[i] = (i + 1) % 13;
        y[i] = 0;
        tmp[i] = 0;
        for (int j = 0; j < N; j++) {
            a[i][j] = (i * (j + 2) + 3) % 17;
        }
    }
}

static void kernel(void) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            tmp[i] += a[i][j] * x[j];
        }
    }
    for (int j = 0; j < N; j++) {
        for (int i = 0; i < N; i++) {
            y[j] += a[i][j] * tmp[i];
        }
    }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++) {
        s += y[i];
    }
    return s;
}

int main(void) {
    init_array();
    kernel();
    fprintf(stderr, "checksum=%lld\n", checksum());
    return (int)(checksum() & 255);
}
