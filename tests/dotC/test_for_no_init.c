// Test for loop with no initialization
int main() {
    int i = 0;
    int sum = 0;

    for (; i < 5; i = i + 1) {
        sum = sum + i;
    }

    return sum;  // 0 + 1 + 2 + 3 + 4 = 10
}

/* Expected: Success - returns 10 */
