// Ackermann function - highly recursive and computationally intensive
int ackermann(int m, int n) {
    if (m == 0) {
        return n + 1;
    }
    if (n == 0) {
        return ackermann(m - 1, 1);
    }
    return ackermann(m - 1, ackermann(m, n - 1));
}

int main() {
    // Keep numbers small to avoid stack overflow
    return ackermann(2, 3);  // Should return 9
}

/* Expected: Success - returns 9 */
