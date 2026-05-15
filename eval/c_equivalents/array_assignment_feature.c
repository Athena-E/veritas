
static void fill_squares(int arr[5]) {
    for (int i = 0; i < 5; i++) {
        arr[i] = i * i;
    }
}

int main(void) {
    int squares[5] = {0};
    fill_squares(squares);
    return squares[3] + squares[4];
}
