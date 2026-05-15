


static long long a[N][N];
static long long x1[N];
static long long x2[N];
static long long y1[N];
static long long y2[N];


static void init_array(void) {
    for (int i = 0; i < N; i++) {
        X1(i) = i % 7;
        X2(i) = (i + 1) % 11;
        Y1(i) = (i + 3) % 13;
        Y2(i) = (i + 5) % 17;
        for (int j = 0; j < N; j++) {
            A(i, j) = (i * (j + 1) + 1) % 19;
        }
    }
}

static void kernel(void) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            X1(i) += A(i, j) * Y1(j);
        }
    }
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            X2(i) += A(j, i) * Y2(j);
        }
    }
}

static long long checksum(void) {
    long long s = 0;
    for (int i = 0; i < N; i++) {
        s += X1(i) + X2(i);
    }
    return s;
}

int main(void) {
    init_array();
    kernel();
    fprintf(stderr, "checksum=%lld\n", checksum());
    return (int)(checksum() & 255);
}
