// Test nested if inside while loop
int main() {
    int sum = 0;
    int i = 0;

    while (i < 10) {
        if (i < 5) {
            sum = sum + 1;
        } else {
            sum = sum + 2;
        }
        i = i + 1;
    }

    // First 5 iterations: add 1 each (5 total)
    // Last 5 iterations: add 2 each (10 total)
    return sum;  // Should be 15
}

/* Expected: Success - returns 15 */
