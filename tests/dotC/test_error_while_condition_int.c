// Test error: using int as while condition
int main() {
    int x = 5;
    while (x) {  // Error: condition must be bool, not int
        x = x - 1;
    }
    return 0;
}

/* Expected: Compilation Error - while condition must be bool */
