// Test less than or equal with floats
int main() {
    float x = 3.14;
    bool result1 = x <= 3.14;  // Should be true (equal)
    bool result2 = x <= 4.0;   // Should be true (less than)
    bool result3 = x <= 2.0;   // Should be false

    if (result1 && result2 && !result3) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
