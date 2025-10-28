// Test deeply nested if-else chain
int main() {
    int x = 15;
    int result = 0;

    if (x < 10) {
        result = 1;
    } else {
        if (x < 20) {
            result = 2;
        } else {
            if (x < 30) {
                result = 3;
            } else {
                result = 4;
            }
        }
    }

    return result;  // Should be 2 (10 <= 15 < 20)
}

/* Expected: Success - returns 2 */
