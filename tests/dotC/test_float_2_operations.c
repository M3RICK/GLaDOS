float calculate() {
    float a = 10.5;
    float b = 2.5;

    float sum = a + b;      // 13.0
    float diff = a - b;     // 8.0
    float prod = a * b;     // 26.25
    float quot = a / b;     // 4.2

    return quot;
}

float main() {
    return calculate();  // Should return 4.2
}

/* Expected: Success */
