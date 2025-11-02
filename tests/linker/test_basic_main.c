// LinkTest: main
// LinkWith: test_basic_lib.c
// Expected: Success (return 16)
// Main program that uses library functions
int add(int a, int b);
int multiply(int x, int y);

int main() {
    int result;
    result = add(5, 3);
    result = multiply(result, 2);
    return result;
}
