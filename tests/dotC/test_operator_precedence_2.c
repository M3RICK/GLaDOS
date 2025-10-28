// Test operator precedence: division before subtraction
int main() {
    int result = 20 - 10 / 2;  // Should be 20 - 5 = 15, not (20 - 10) / 2 = 5
    return result;
}

/* Expected: Success - returns 15 */
