

void bubble_sort(int arr[], int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int tmp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tmp;
            }
        }
    }
}

int main(void) {
    int arr[10] = {5, 12, 3, 42, 17, 8, 91, 23, 56, 4};
    bubble_sort(arr, 10);
    return arr[0];
}
