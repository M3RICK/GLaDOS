// Test error: using void function result in expression
void doNothing() {
    int x = 5;
}

int main() {
    int result = doNothing();  // Error: can't use void result
    return result;
}

/* Expected: Compilation Error - void function in expression */
