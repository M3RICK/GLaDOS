# Your First Program

Now that you have GLaDOS installed, let's write your first program!

## Hello World

Create a file called `hello.c`:

```c
int main() {
    int x = 5;
    int y = 10;
    int result = x + y;
    return result;
}
```

### Compile and Run

```bash
./glados < hello.c
```

**Expected output:**
```
VInt 15
```

Congratulations! You've just compiled and run your first GLaDOS program. The program adds two numbers and returns the result.

## Next Steps

Now that you've written your first programs, you can:

- Explore more [Examples](./examples.md)
- Learn the full [Syntax Guide](../language/syntax.md)
- Understand the [Type System](../language/types.md)
