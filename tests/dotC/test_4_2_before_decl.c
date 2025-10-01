int main() {
    int y = x + 1;  // x used before declaration
    int x = 5;
    return y;
}

/* Expected: Undefined variable error */
