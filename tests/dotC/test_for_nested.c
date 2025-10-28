// Test nested for loops
int main() {
    int sum = 0;

    for (int i = 1; i <= 3; i = i + 1) {
        for (int j = 1; j <= 3; j = j + 1) {
            sum = sum + i * j;
        }
    }

    // i=1: j=1,2,3 -> 1+2+3=6
    // i=2: j=1,2,3 -> 2+4+6=12
    // i=3: j=1,2,3 -> 3+6+9=18
    // Total: 6+12+18 = 36
    return sum;
}

/* Expected: Success - returns 36 */
