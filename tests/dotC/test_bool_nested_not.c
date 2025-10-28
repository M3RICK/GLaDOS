// Test nested NOT operators
int main() {
    bool value = true;
    bool result = !!value;  // Double negation should give back true

    if (result) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
