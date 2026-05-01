// C equivalent of src/examples/22_bubble_sort.veri
// Sorts five integers, computes their sum, and returns the integer average.

static void sort(int arr[5]) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            if (arr[j] > arr[j + 1]) {
                int tmp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tmp;
            }
        }
    }
}

int main(void) {
    int arr[5] = {34, 7, 22, 3, 15};
    int sum = 0;

    sort(arr);
    for (int i = 0; i < 5; i++) {
        sum += arr[i];
    }

    return sum / 5;
}
