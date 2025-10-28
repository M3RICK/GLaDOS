// Test complex nested control structures
int main() {
    int result = 0;

    for (int i = 0; i < 5; i = i + 1) {
        int j = 0;
        while (j < 3) {
            if (i == j) {
                result = result + 1;
            } else {
                if (i > j) {
                    result = result + 2;
                }
            }
            j = j + 1;
        }
    }

    // i=0: j=0,1,2 -> 1+0+0=1
    // i=1: j=0,1,2 -> 2+1+0=3
    // i=2: j=0,1,2 -> 2+2+1=5
    // i=3: j=0,1,2 -> 2+2+2=6
    // i=4: j=0,1,2 -> 2+2+2=6
    // Total: 1+3+5+6+6 = 21
    return result;
}

/* Expected: Success - returns 21 */
