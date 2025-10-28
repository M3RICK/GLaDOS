int main() {
    int sum = 0;
    int i = 1;
    while (i <= 3) {
        int j = 1;
        while (j <= 3) {
            sum = sum + (i * j);
            j = j + 1;
        }
        i = i + 1;
    }
    return sum;  // (1*1 + 1*2 + 1*3) + (2*1 + 2*2 + 2*3) + (3*1 + 3*2 + 3*3) = 36
}

/* Expected: Success - returns 36 */
