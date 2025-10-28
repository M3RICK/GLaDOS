// Test operator precedence: multiplication before addition
int main() {
    int result = 2 + 3 * 4;  // Should be 2 + 12 = 14, not (2 + 3) * 4 = 20
    return result;
}

/* Expected: Success - returns 14 */
