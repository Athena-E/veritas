



static long long a[N][N];
static long long b[N][N];


static void init_array(void) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            A(i, j) = (i * (j + 2) + 2) / N;
            B(i, j) = (i * (j + 3) + 3) / N;
        }
    }
}

static void kernel(void) {
    for (int t = 0; t < TSTEPS; t++) {
        for (int i = 1; i < N - 1; i++)
            for (int j = 1; j < N - 1; j++)
                B(i, j) = (A(i, j) + A(i, j - 1) + A(i, j + 1) + A(i + 1, j) + A(i - 1, j)) / 5;
        for (int i = 1; i < N - 1; i++)
            for (int j = 1; j < N - 1; j++)
                A(i, j) = B(i, j);
    }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++)
        for (int j = 0; j < N; j++)
            s += A(i, j);
    return s;
}

int main(void) {
    init_array();
    kernel();
    fprintf(stderr, "checksum=%lld\n", checksum());
    return (int)(checksum() & 255);
}
