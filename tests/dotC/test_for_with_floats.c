// Test for loop with float counter (unconventional but valid)
float main() {
    float sum = 0.0;

    for (float f = 1.0; f < 5.0; f = f + 1.0) {
        sum = sum + f;
    }

    return sum;  // 1.0 + 2.0 + 3.0 + 4.0 = 10.0
}

/* Expected: Success - returns 10.0 */
