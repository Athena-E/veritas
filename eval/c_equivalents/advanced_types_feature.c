
static int use_positive(int n) {
    return n;
}

int main(void) {
    int answer = 42;
    int positive = 5;
    int repeated[3] = {answer, answer, answer};
    return use_positive(positive) + repeated[0];
}
