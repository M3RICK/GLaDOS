// LinkTest: main
// LinkWith: test_float_lib.c
// Expected: Success (return 0)
// Main using float functions
float addFloat(float a, float b);
float multiplyFloat(float a, float b);

int main() {
    float result;
    result = addFloat(3.5, 2.5);
    result = multiplyFloat(result, 2.0);
    return 0;
}
