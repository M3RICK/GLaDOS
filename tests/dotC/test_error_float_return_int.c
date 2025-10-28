// Test error: returning float from int function
int getValue() {
    return 3.14;  // Error: returning float from int function
}

int main() {
    return getValue();
}

/* Expected: Compilation Error - return type mismatch */
