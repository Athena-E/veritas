int main(void) {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            int product = i * j;
            (void)product;
        }
    }

    for (int i = 1; i < 100; i++) {
        int x = i * 2;
        int y = x + 5;
        int z = y * y;
        (void)z;
    }

    int arr[10] = {0};
    for (int i = 0; i < 10; i++) {
        arr[i] = i * i;
    }

    return 0;
}
