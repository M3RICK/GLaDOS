# Examples

This page contains practical examples demonstrating various GLaDOS features.

## Basic Programs

### Hello World (Simple Return)

```c
int main() {
    return 42;
}
```

Output: `42`

### Simple Addition

```c
int add(int a, int b) {
    return a + b;
}

int main() {
    return add(5, 3);
}
```

Output: `8`

### Variable Declaration and Assignment

```c
int main() {
    int x = 10;
    int y = 20;
    int z = x + y;
    return z;
}
```

Output: `30`

## Control Flow Examples

### If-Else Statement

```c
int max(int a, int b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}

int main() {
    int result = max(10, 20);
    return result;
}
```

Output: `20`

### Nested If Statements

```c
int abs(int x) {
    if (x < 0) {
        return -x;
    } else {
        return x;
    }
}

int main() {
    return abs(-15);
}
```

Output: `15`

### While Loop

```c
int sum_to_n(int n) {
    int sum = 0;
    int i = 1;

    while (i <= n) {
        sum = sum + i;
        i = i + 1;
    }
    return sum;
}

int main() {
    return sum_to_n(10);  // 1+2+3+...+10
}
```

Output: `55`

## Recursive Functions

### Factorial

```c
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main() {
    int result = factorial(5);
    return result;
}
```

Output: `120` (5! = 5×4×3×2×1)

### Fibonacci

```c
int fib(int n) {
    if (n <= 1) {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

int main() {
    return fib(10);
}
```

Output: `55`

### Power Function

```c
int power(int base, int exp) {
    if (exp == 0) {
        return 1;
    }
    return base * power(base, exp - 1);
}

int main() {
    return power(2, 8);  // 2^8
}
```

Output: `256`

## Boolean Logic

### Comparison Functions

```c
bool isPositive(int x) {
    return x > 0;
}

bool isNegative(int x) {
    return x < 0;
}

int main() {
    bool result = isPositive(5);
    if (result) {
        return 1;
    }
    return 0;
}
```

Output: `1`

### Range Checking

```c
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
```

Output: `1`

### Logical Operations

```c
int main() {
    bool a = 5 < 10;       // true
    bool b = 10 == 10;     // true
    bool c = a && b;       // true

    if (c) {
        return 1;
    }
    return 0;
}
```

Output: `1`

## Function Composition

### Nested Function Calls

```c
int double(int x) {
    return x * 2;
}

int quadruple(int x) {
    return double(double(x));
}

int main() {
    return quadruple(5);
}
```

Output: `20`

### Helper Functions

```c
int square(int x) {
    return x * x;
}

int sumOfSquares(int a, int b) {
    return square(a) + square(b);
}

int main() {
    return sumOfSquares(3, 4);  // 3² + 4² = 9 + 16
}
```

Output: `25`

## Iterative vs Recursive

### Factorial (Iterative)

```c
int factorial_iter(int n) {
    int result = 1;
    while (n > 0) {
        result = result * n;
        n = n - 1;
    }
    return result;
}

int main() {
    return factorial_iter(5);
}
```

Output: `120`

### Factorial (Recursive)

```c
int factorial_rec(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial_rec(n - 1);
}

int main() {
    return factorial_rec(5);
}
```

Output: `120`

## Advanced Examples

### Greatest Common Divisor (GCD)

```c
int gcd(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a - (a / b) * b;  // a mod b
        a = temp;
    }
    return a;
}

int main() {
    return gcd(48, 18);
}
```

Output: `6`

### Count Digits

```c
int countDigits(int n) {
    int count = 0;

    if (n == 0) {
        return 1;
    }

    while (n != 0) {
        n = n / 10;
        count = count + 1;
    }
    return count;
}

int main() {
    return countDigits(12345);
}
```

Output: `5`

### Reverse Number

```c
int reverse(int n) {
    int reversed = 0;

    while (n != 0) {
        int digit = n - (n / 10) * 10;  // n mod 10
        reversed = reversed * 10 + digit;
        n = n / 10;
    }
    return reversed;
}

int main() {
    return reverse(12345);
}
```

Output: `54321`

## Expression Statements

Function calls can be used as statements (result discarded):

```c
int print(int x) {
    return x;  // Pretend this prints
}

int main() {
    print(42);  // Expression statement
    print(100);
    return 0;
}
```

Output: `0`

## Tips for Writing GLaDOS Programs

1. **Always initialize variables** before using them
2. **Ensure all code paths return** in non-void functions
3. **Use recursion** for naturally recursive problems
4. **Use loops** when you need state mutation
5. **Break complex logic** into smaller functions
6. **Test edge cases** like 0, negative numbers, etc.
