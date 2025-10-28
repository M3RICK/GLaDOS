// Test error: comparing different types
int main() {
    int x = 5;
    float y = 5.0;
    bool result = x < y;  // Error: can't compare int and float
    return 0;
}

/* Expected: Compilation Error - type mismatch in comparison */
