
static int touch(int arr[1]) {
    arr[0] = 9;
    return arr[0];
}

int main(void) {
    int arr[1] = {0};
    return touch(arr);
}
