



static long long a[N];
static long long b[N];


static void init_array(void) {
    for (int i = 0; i < N; i++) {
        A(i) = (i + 2) / N;
        B(i) = (i + 3) / N;
    }
}

static void kernel(void) {
    for (int t = 0; t < TSTEPS; t++) {
        for (int i = 1; i < N - 1; i++)
            B(i) = (A(i - 1) + A(i) + A(i + 1)) / 3;
        for (int j = 1; j < N - 1; j++)
            A(j) = B(j);
    }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++)
        s += A(i);
    return s;
}

int main(void) {
    init_array();
    kernel();
    fprintf(stderr, "checksum=%lld\n", checksum());
    return (int)(checksum() & 255);
}
