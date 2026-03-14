// C equivalent of 14_for_loops.veri - complex_loop
int main(void) {
    // nested_loops
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            int product = i * j;
            (void)product;
        }
    }

    // complex_loop
    for (int i = 1; i < 100; i++) {
        int x = i * 2;
        int y = x + 5;
        int z = y * y;
        (void)z;
    }

    // array_loop
    int arr[10] = {0};
    for (int i = 0; i < 10; i++) {
        arr[i] = i * i;
    }

    // 14_for_loops.veri has no main(), exit code is arbitrary
    // We just compare code size, not runtime output
    return 0;
}
