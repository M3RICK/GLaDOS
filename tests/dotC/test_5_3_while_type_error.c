int main() {
    int x = 5;
    while (x) {  // int where bool expected
        x = x - 1;
    }
    return 0;
}

/* Expected: Type error in while condition */
