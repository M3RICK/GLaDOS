// Test complex AND expression with multiple conditions
int main() {
    int x = 10;
    int y = 20;
    int z = 30;

    bool result = x < y && y < z && x < z;

    if (result) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
