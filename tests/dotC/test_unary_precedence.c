// Test unary operator precedence
int main() {
    int x = 5;
    int result = -x * 2;  // Should be (-5) * 2 = -10, not -(5 * 2) = -10 (same result but tests precedence)
    return result;
}

/* Expected: Success - returns -10 */
