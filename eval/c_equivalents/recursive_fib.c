// Recursive fibonacci - compute-intensive benchmark
// No direct .veri equivalent but tests function call overhead
int fib(int n) {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
}

int main(void) {
    return fib(30) % 256;
}
