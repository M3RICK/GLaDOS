// Test greater than or equal operator
int main() {
    int x = 10;
    bool result1 = x >= 10;  // Should be true
    bool result2 = x >= 9;   // Should be true
    bool result3 = x >= 11;  // Should be false

    if (result1 && result2 && !result3) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
