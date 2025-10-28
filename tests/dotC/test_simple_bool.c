int main() {
    bool x = true;
    bool y = false;
    if (x && !y) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
