// LinkTest: lib for test_multi_main.c
// Helper library that uses math functions
int square(int n);
int cube(int n);

int sumOfSquares(int a, int b) {
    return square(a) + square(b);
}

int sumOfCubes(int a, int b) {
    return cube(a) + cube(b);
}
