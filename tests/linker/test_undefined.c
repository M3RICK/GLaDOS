// LinkTest: standalone
// Expected: Error - undefined function
// Main that calls an undefined function
int undefinedFunction(int x);

int main() {
    return undefinedFunction(42);
}
