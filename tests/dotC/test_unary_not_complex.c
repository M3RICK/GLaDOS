// Test NOT operator in complex expression
int main() {
    int x = 5;
    int y = 10;
    bool result = !(x > y);  // x > y is false, so !(false) is true

    if (result) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
