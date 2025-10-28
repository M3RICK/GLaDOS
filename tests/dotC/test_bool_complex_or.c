// Test complex OR expression with multiple conditions
int main() {
    int x = 10;

    bool result = x < 5 || x > 20 || x == 10;  // Last one is true

    if (result) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
