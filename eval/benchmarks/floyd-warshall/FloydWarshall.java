public class FloydWarshall {
    static final int N = 1024;
    static long[][] path = new long[N][N];

    static void initArray() {
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                path[i][j] = ((long)(i + 1) * (j + 1)) / N;
    }

    static void kernel() {
        for (int k = 0; k < N; k++)
            for (int i = 0; i < N; i++)
                for (int j = 0; j < N; j++) {
                    long via = path[i][k] + path[k][j];
                    if (via < path[i][j]) path[i][j] = via;
                }
    }

    static long checksum() {
        long s = 0;
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                s += path[i][j];
        return s;
    }

    public static void main(String[] args) {
        initArray();
        long t0 = System.nanoTime();
        kernel();
        double elapsed = (System.nanoTime() - t0) / 1e9;
        System.err.println("checksum=" + checksum());
        System.out.printf("%.6f%n", elapsed);
    }
}
