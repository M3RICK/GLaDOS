// Prime number checker with optimization
bool isPrime(int n) {
    if (n <= 1) {
        return false;
    }
    if (n == 2) {
        return true;
    }
    if (n / 2 == 0) {
        return false;
    }

    int i = 3;
    while (i * i <= n) {
        if (n / i == 0) {
            return false;
        }
        i = i + 2;
    }
    return true;
}

int countPrimes(int limit) {
    int count = 0;
    int i = 2;
    while (i <= limit) {
        if (isPrime(i)) {
            count = count + 1;
        }
        i = i + 1;
    }
    return count;
}

int main() {
    return countPrimes(20);  // 2,3,5,7,11,13,17,19 = 8 primes
}

/* Expected: Success - returns 8 */
