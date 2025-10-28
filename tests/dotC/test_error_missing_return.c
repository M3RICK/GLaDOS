// Test error: non-void function missing return statement
int compute(int x) {
    int y = x * 2;
    // Missing return statement!
}

int main() {
    return compute(5);
}

/* Expected: Compilation Error - missing return statement */
