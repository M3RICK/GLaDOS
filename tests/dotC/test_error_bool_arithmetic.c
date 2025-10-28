// Test error: trying to use arithmetic operators on booleans
int main() {
    bool a = true;
    bool b = false;
    bool result = a + b;  // Error: can't add booleans
    return 0;
}

/* Expected: Compilation Error - type mismatch for arithmetic */
