// Test De Morgan's law: !(a && b) == !a || !b
int main() {
    bool a = true;
    bool b = false;

    bool left = !(a && b);
    bool right = !a || !b;

    // Both should be true
    if (left && right) {
        return 1;
    }
    return 0;
}

/* Expected: Success - returns 1 */
