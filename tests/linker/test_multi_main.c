// LinkTest: main
// LinkWith: test_math_lib.c test_helper_lib.c
// Expected: Error - forward declarations not fully supported in --compile-obj mode yet
// Main program using multiple libraries
int sumOfSquares(int a, int b);
int sumOfCubes(int a, int b);

int main() {
    int result;
    result = sumOfSquares(3, 4);
    result = result + sumOfCubes(1, 2);
    return result;
}
