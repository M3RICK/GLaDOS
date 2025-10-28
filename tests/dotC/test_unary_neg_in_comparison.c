// Test negation in comparison
int main() {
    int x = 5;
    bool result = -x < 0;  // -5 < 0 should be true

    if (result) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
