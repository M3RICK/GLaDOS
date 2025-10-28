// Test error: trying to use logical operators on integers
int main() {
    int a = 5;
    int b = 10;
    int result = a && b;  // Error: && requires bool operands
    return result;
}

/* Expected: Compilation Error - type mismatch for logical operator */
