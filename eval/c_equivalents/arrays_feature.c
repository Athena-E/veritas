
static int endpoint_sum(const int arr[4]) {
    return arr[0] + arr[3];
}

int main(void) {
    int arr[4] = {5, 5, 5, 5};
    arr[3] = 7;
    return endpoint_sum(arr);
}
