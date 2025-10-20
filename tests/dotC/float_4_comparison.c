bool isGreater(float a, float b) {
    return a > b;
}

bool isEqual(float a, float b) {
    return a == b;
}

float main() {
    bool test1 = isGreater(3.14, 2.71);  // true
    bool test2 = isEqual(1.0, 1.0);      // true

    if (test1 && test2) {
        return 1.0;
    }
    return 0.0;
}

/* Expected: Success - returns 1.0 */
