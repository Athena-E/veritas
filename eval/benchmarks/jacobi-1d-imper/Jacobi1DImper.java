public class Jacobi1DImper {
    static final int N = 128;
    static final int TSTEPS = 80;
    static long[] a = new long[N];
    static long[] b = new long[N];

    static void initArray() {
        for (int i = 0; i < N; i++) {
            a[i] = (i + 2) / N;
            b[i] = (i + 3) / N;
        }
    }

    static void kernel() {
        for (int t = 0; t < TSTEPS; t++) {
            for (int i = 1; i < N - 1; i++) {
                b[i] = (a[i - 1] + a[i] + a[i + 1]) / 3;
            }
            for (int j = 1; j < N - 1; j++) {
                a[j] = b[j];
            }
        }
    }

    static long checksum() {
        long s = 0;
        for (int i = 0; i < N; i++) {
            s += a[i];
        }
        return s;
    }

    public static void main(String[] args) {
        initArray();
        kernel();
        System.err.println("checksum=" + checksum());
    }
}
