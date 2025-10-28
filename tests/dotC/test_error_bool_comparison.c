// Test error: using < or > on booleans
int main() {
    bool a = true;
    bool b = false;
    bool result = a < b;  // Error: < not defined for bool
    return 0;
}

/* Expected: Compilation Error - invalid operator for bool */
