

static long long a[N][N];
static long long b[N][N];
static long long x[N];
static long long y[N];
static const long long alpha = 3;
static const long long beta = 2;

static void init_array(void) {
    for (int i = 0; i < N; i++) {
        x[i] = (i + 1) % 11;
        y[i] = 0;
        for (int j = 0; j < N; j++) {
            a[i][j] = (i * (j + 1) + 1) % 13;
            b[i][j] = (i * (j + 2) + 2) % 17;
        }
    }
}

static void kernel(void) {
    for (int i = 0; i < N; i++) {
        long long tmp1 = 0;
        long long tmp2 = 0;
        for (int j = 0; j < N; j++) {
            tmp1 += a[i][j] * x[j];
            tmp2 += b[i][j] * x[j];
        }
        y[i] = alpha * tmp1 + beta * tmp2;
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
