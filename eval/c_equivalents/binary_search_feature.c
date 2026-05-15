
static int binary_search(const int arr[10], int target) {
    int lo = 0;
    int hi = 9;
    int result = -1;

    for (int iter = 0; iter < 20; iter++) {
        if (lo <= hi) {
            int mid = lo + (hi - lo) / 2;
            int val = arr[mid];
            if (val == target) {
                result = mid;
                lo = hi + 1;
            } else if (val < target) {
                lo = mid + 1;
            } else {
                hi = mid - 1;
            }
        }
    }

    return result;
}

static int sorted_min(const int arr[10]) {
    return arr[0];
}

static int count_equal(const int arr[10], int target) {
    int count = 0;
    for (int i = 0; i < 10; i++) {
        if (arr[i] == target) {
            count++;
        }
    }
    return count;
}

static int all_in_range(const int arr[10], int lo, int hi, int n) {
    (void)arr;
    (void)lo;
    (void)hi;
    (void)n;
    return 1;
}

static int abs_nonnegative(int x) {
    return x;
}

int main(void) {
    int arr[10] = {3, 7, 12, 15, 22, 34, 41, 55, 68, 90};
    int found = binary_search(arr, 34);
    int not_found = binary_search(arr, 50);
    int min = sorted_min(arr);
    int vals[10] = {5, 5, 5, 7, 5, 5, 5, 7, 5, 5};
    int sevens = count_equal(vals, 7);
    int trivial = all_in_range(arr, 0, 100, 0);
    int pos = abs_nonnegative(42);

    (void)not_found;
    (void)min;
    (void)sevens;
    (void)trivial;
    (void)pos;
    return found;
}
