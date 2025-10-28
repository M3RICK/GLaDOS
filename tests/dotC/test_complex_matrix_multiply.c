// 2x2 Matrix multiplication
int main() {
    // Matrix A
    int a11 = 1;
    int a12 = 2;
    int a21 = 3;
    int a22 = 4;

    // Matrix B
    int b11 = 5;
    int b12 = 6;
    int b21 = 7;
    int b22 = 8;

    // Result matrix C = A * B
    int c11 = a11 * b11 + a12 * b21;  // 1*5 + 2*7 = 19
    int c12 = a11 * b12 + a12 * b22;  // 1*6 + 2*8 = 22
    int c21 = a21 * b11 + a22 * b21;  // 3*5 + 4*7 = 43
    int c22 = a21 * b12 + a22 * b22;  // 3*6 + 4*8 = 50

    return c11 + c12 + c21 + c22;     // 19 + 22 + 43 + 50 = 134
}

/* Expected: Success - returns 134 */
