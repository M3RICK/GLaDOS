bool isInRange(int x, int low, int high) {
    return (x >= low) && (x <= high);
}

int main() {
    bool result = isInRange(5, 1, 10);
    if (result) {
        return 1;
    }
    return 0;
}

/* Expected: Success */
