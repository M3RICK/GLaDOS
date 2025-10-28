// Test complex boolean expression with mixed operators
int main() {
    int x = 5;
    int y = 10;
    int z = 15;

    // (x < y && y < z) || (x == 0)
    // (true && true) || false
    // true || false = true
    bool result = (x < y && y < z) || (x == 0);

    if (result) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
