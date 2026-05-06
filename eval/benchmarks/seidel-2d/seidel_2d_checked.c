#include <stdio.h>

#include "../checked_bounds.h"

#ifndef N
#define N 120
#endif

#ifndef TSTEPS
#define TSTEPS 40
#endif

static long long a[N][N];

#define A(i, j) a[CHECKED_IDX1("a", (i), N)][CHECKED_IDX1("a", (j), N)]

static void init_array(void) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            A(i, j) = (i * (j + 2) + 2) % 17;
        }
    }
}

static void kernel(void) {
    for (int t = 0; t < TSTEPS; t++) {
        for (int i = 1; i < N - 1; i++) {
            for (int j = 1; j < N - 1; j++) {
                A(i, j) =
                    (A(i - 1, j - 1) + A(i - 1, j) + A(i - 1, j + 1) +
                     A(i, j - 1) + A(i, j) + A(i, j + 1) +
                     A(i + 1, j - 1) + A(i + 1, j) + A(i + 1, j + 1)) / 9;
            }
        }
    }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            s += A(i, j);
        }
    }
    return s;
}

int main(void) {
    init_array();
    kernel();
    fprintf(stderr, "checksum=%lld\n", checksum());
    return (int)(checksum() & 255);
}
