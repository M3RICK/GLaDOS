// Test operator precedence: comparison before logical AND
int main() {
    bool result = 5 < 10 && 10 > 3;  // Should evaluate comparisons first
    if (result) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
