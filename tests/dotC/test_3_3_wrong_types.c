int add(int a, int b) {
    return a + b;
}

int main() {
    bool x = true;
    return add(x, 5);  // bool passed where int expected
}

/* Expected: Type mismatch in function argument */
