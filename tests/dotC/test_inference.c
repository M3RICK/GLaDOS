int main() {
    var x = 42;           // Infers int
    var flag = true      // Infers bool
    var sum = x + 8;      // Infers int

    var result = sum * 2 // Infers int

    return result;        // Should return 100
}
