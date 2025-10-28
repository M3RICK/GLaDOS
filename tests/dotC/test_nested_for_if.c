// Test if statement inside for loop
int main() {
    int sum = 0;

    for (int i = 1; i <= 10; i = i + 1) {
        if (i < 5 || i > 8) {
            sum = sum + i;
        }
    }

    // Add: 1, 2, 3, 4, 9, 10
    return sum;  // 1+2+3+4+9+10 = 29
}

/* Expected: Success - returns 29 */
