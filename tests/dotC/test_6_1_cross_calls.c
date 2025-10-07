int double(int x) {
    return x * 2;
}

int quadruple(int x) {
    return double(double(x));
}

int main() {
    return quadruple(5);
}

/* Expected: Success */
