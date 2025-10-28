// Test error: returning wrong type from function
int getValue() {
    return true;  // Error: returning bool from int function
}

int main() {
    return getValue();
}

/* Expected: Compilation Error - return type mismatch */
