// Test operator precedence: AND before OR
int main() {
    // false || true && false evaluates as: false || (true && false) = false || false = false
    bool result = false || true && false;
    if (result) {
        return 1;
    }
    return 0;  // Should return 0
}

/* Expected: Success - returns 0 */
