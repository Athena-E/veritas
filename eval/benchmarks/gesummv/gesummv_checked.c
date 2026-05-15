


static long long a[N][N];
static long long b[N][N];
static long long x[N];
static long long y[N];
static const long long alpha = 3;
static const long long beta = 2;


static void init_array(void) {
    for (int i = 0; i < N; i++) {
        X(i) = (i + 1) % 11;
        Y(i) = 0;
        for (int j = 0; j < N; j++) {
            A(i, j) = (i * (j + 1) + 1) % 13;
            B(i, j) = (i * (j + 2) + 2) % 17;
        }
    }
}

static void kernel(void) {
    for (int i = 0; i < N; i++) {
        long long tmp1 = 0;
        long long tmp2 = 0;
        for (int j = 0; j < N; j++) {
            tmp1 += A(i, j) * X(j);
            tmp2 += B(i, j) * X(j);
        }
        Y(i) = alpha * tmp1 + beta * tmp2;
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
