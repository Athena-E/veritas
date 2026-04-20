#include <stdio.h>

#ifndef N
#define N 128
#endif

#ifndef TSTEPS
#define TSTEPS 80
#endif

static long long a[N];
static long long b[N];

static void init_array(void) {
    for (int i = 0; i < N; i++) {
        a[i] = (i + 2) / N;
        b[i] = (i + 3) / N;
    }
}

static void kernel(void) {
    for (int t = 0; t < TSTEPS; t++) {
        for (int i = 1; i < N - 1; i++)
            b[i] = (a[i - 1] + a[i] + a[i + 1]) / 3;
        for (int j = 1; j < N - 1; j++)
            a[j] = b[j];
    }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++)
        s += a[i];
    return s;
}

int main(void) {
    init_array();
    kernel();
    fprintf(stderr, "checksum=%lld\n", checksum());
    return (int)(checksum() & 255);
}
