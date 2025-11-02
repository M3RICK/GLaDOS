// LinkTest: standalone
// Expected: Error - undefined functions (no lib provides them)
// Middle layer using base utilities
int increment(int x);
int decrement(int x);

int addTwo(int x) {
    return increment(increment(x));
}

int subTwo(int x) {
    return decrement(decrement(x));
}
