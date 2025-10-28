// Test error: using int as if condition
int main() {
    int x = 5;
    if (x) {  // Error: condition must be bool, not int
        return 1;
    }
    return 0;
}

/* Expected: Compilation Error - if condition must be bool */
