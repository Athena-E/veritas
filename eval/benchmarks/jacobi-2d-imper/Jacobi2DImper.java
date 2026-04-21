public class Jacobi2DImper {
    static final int N = 64;
    static final int TSTEPS = 20;
    static long[][] a = new long[N][N];
    static long[][] b = new long[N][N];

    static void initArray() {
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                a[i][j] = (i * (j + 2) + 2) / N;
                b[i][j] = (i * (j + 3) + 3) / N;
            }
        }
    }

    static void kernel() {
        for (int t = 0; t < TSTEPS; t++) {
            for (int i = 1; i < N - 1; i++) {
                for (int j = 1; j < N - 1; j++) {
                    b[i][j] = (a[i][j] + a[i][j - 1] + a[i][j + 1] + a[i + 1][j] + a[i - 1][j]) / 5;
                }
            }
            for (int i = 1; i < N - 1; i++) {
                for (int j = 1; j < N - 1; j++) {
                    a[i][j] = b[i][j];
                }
            }
        }
    }

    static long checksum() {
        long s = 0;
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                s += a[i][j];
            }
        }
        return s;
    }

    public static void main(String[] args) {
        initArray();
        kernel();
        System.err.println("checksum=" + checksum());
    }
}
