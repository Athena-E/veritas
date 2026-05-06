#ifndef VERITAS_CHECKED_BOUNDS_H
#define VERITAS_CHECKED_BOUNDS_H

#include <stdio.h>
#include <stdlib.h>

static inline int checked_idx1(const char *name, int idx, int upper, const char *file, int line) {
    if (idx < 0 || idx >= upper) {
        fprintf(stderr, "bounds check failed: %s[%d] with upper=%d at %s:%d\n",
                name, idx, upper, file, line);
        abort();
    }
    return idx;
}

#define CHECKED_IDX1(name, idx, upper) checked_idx1((name), (idx), (upper), __FILE__, __LINE__)

#endif
