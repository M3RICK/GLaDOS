// Test for loop with no update statement
int main() {
    int sum = 0;

    for (int i = 0; i < 5; ) {
        sum = sum + i;
        i = i + 1;  // Update inside the loop body
    }

    return sum;  // 0 + 1 + 2 + 3 + 4 = 10
}

/* Expected: Success - returns 10 */
