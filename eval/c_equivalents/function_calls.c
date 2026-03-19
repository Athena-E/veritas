// C equivalent of 07_function_calls.veri
int add(int a, int b) {
    return a + b;
}

int multiply(int a, int b) {
    return a * b;
}

int complex_calc(int x, int y, int z) {
    return x + y * z;
}

int main(void) {
    int x = add(5, 10);
    int y = multiply(x, 2);
    int z = complex_calc(x, y, 100);
    // 07_function_calls.veri: main calls helper() which has no return
    // Veritas returns 0
    (void)z;
    return 0;
}
