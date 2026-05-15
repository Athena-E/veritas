



static long long a[M][N];
static long long x[N];
static long long y[N];
static long long tmp[M];


static void init_array(void) {
    for (int i = 0; i < N; i++) {
        X(i) = (i + 1) % 13;
        Y(i) = 0;
    }
    for (int i = 0; i < M; i++) {
        TMP(i) = 0;
        for (int j = 0; j < N; j++) {
            A(i, j) = (i * (j + 2) + 3) % 17;
        }
    }
}

static void kernel(void) {
    for (int i = 0; i < M; i++) {
        for (int j = 0; j < N; j++) {
            TMP(i) += A(i, j) * X(j);
        }
    }
    for (int j = 0; j < N; j++) {
        for (int i = 0; i < M; i++) {
            Y(j) += A(i, j) * TMP(i);
        }
    }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++) {
        s += Y(i);
    }
    return s;
}

int main(void) {
    init_array();
    kernel();
    fprintf(stderr, "checksum=%lld\n", checksum());
    return (int)(checksum() & 255);
}
