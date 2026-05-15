
static int sorted_head(const int arr[3]) {
    return arr[0];
}

static void make_sorted(int arr[3]) {
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;
}

int main(void) {
    int arr[3] = {0, 0, 0};
    make_sorted(arr);
    return sorted_head(arr);
}
