// Test for loop counting down
int main() {
    int sum = 0;

    for (int i = 10; i > 0; i = i - 1) {
        sum = sum + i;
    }

    return sum;  // 10+9+8+7+6+5+4+3+2+1 = 55
}

/* Expected: Success - returns 55 */
