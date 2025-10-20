float main() {
    var x = 3.14;        // Should infer float
    var y = 2.0      // Should infer float
    var sum = x + y     // Should infer float

    return sum;  // 5.14
}

/* Expected: Success */
