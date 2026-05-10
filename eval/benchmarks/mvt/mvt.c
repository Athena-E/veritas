#include <stdio.h>

#ifndef N
#define N 120
#endif

static long long a[N][N];
static long long x1[N];
static long long x2[N];
static long long y1[N];
static long long y2[N];

static void init_array(void) {
    for (int i = 0; i < N; i++) {
        x1[i] = i % 7;
        x2[i] = (i + 1) % 11;
        y1[i] = (i + 3) % 13;
        y2[i] = (i + 5) % 17;
        for (int j = 0; j < N; j++) {
            a[i][j] = (i * (j + 1) + 1) % 19;
        }
    }
}

static void kernel(void) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            x1[i] += a[i][j] * y1[j];
        }
    }
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            x2[i] += a[j][i] * y2[j];
        }
    }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++) {
        s += x1[i] + x2[i];
    }
    return s;
}

int main(void) {
    init_array();
    kernel();
    fprintf(stderr, "checksum=%lld\n", checksum());
    return (int)(checksum() & 255);
}
