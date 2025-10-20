int bad1(int x) {
    if (x > 0) {
        return 1;
    }
    // Error: missing return
}

int bad2(int x) {
    while (x > 0) {
        return 1;  // Doesn't count - loop might not run
    }
    // Error: missing return
}
