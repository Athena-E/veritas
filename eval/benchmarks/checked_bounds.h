

static inline int checked_idx1(const char *name, int idx, int upper, const char *file, int line) {
    if (idx < 0 || idx >= upper) {
        fprintf(stderr, "bounds check failed: %s[%d] with upper=%d at %s:%d\n",
                name, idx, upper, file, line);
        abort();
    }
    return idx;
}


