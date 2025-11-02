// LinkTest: main
// LinkWith: test_dup1.c
// Expected: Error - duplicate function definition
// Second library with foo function (duplicate!)
int foo() {
    return 99;
}

int main() {
    return foo();
}
