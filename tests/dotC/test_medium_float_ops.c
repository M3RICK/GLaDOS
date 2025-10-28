float average(float a, float b, float c) {
    return (a + b + c) / 3.0;
}

float main() {
    float x = 10.5;
    float y = 20.5;
    float z = 30.0;
    float avg = average(x, y, z);
    return avg;  // 20.333...
}

/* Expected: Success - returns ~20.33 */
