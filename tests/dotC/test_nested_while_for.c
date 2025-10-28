// Test for loop inside while loop
int main() {
    int sum = 0;
    int outer = 0;

    while (outer < 3) {
        for (int inner = 0; inner < 3; inner = inner + 1) {
            sum = sum + 1;
        }
        outer = outer + 1;
    }

    return sum;  // 3 outer iterations * 3 inner = 9
}

/* Expected: Success - returns 9 */
