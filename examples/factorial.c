// 0 - <, 1 - =, 2 - >
int compare(int a, int b) {
    while (b) {
        if (a) {} else {
            return 0;
        }

        a = a - 1;
        b = b - 1;
    }

    if (a) {
        return 2;
    } else {
        return 1;
    }
}

int compare_bool(int a, int b, int r) {
    if (compare(a, b) - r) {
        return 0;
    } else {
        return 1;
    }
}

int lor(int a, int b) {
    if (a) { return 1; }
    else { if (b) { return 1; } else { return 0; } }
}

int land(int a, int b) {
    if (a) { if (b) { return 1; } else { return 0; } }
    else { return 0; }
}

int lnot(int a) {
    if (a) { return 0; } else { return 1; }
}

int lt(int a, int b) { return compare_bool(a, b, 0); }
int eq(int a, int b) { return compare_bool(a, b, 1); }
int gt(int a, int b) { return compare_bool(a, b, 2); }

int le(int a, int b) { return lor(lt(a, b), eq(a, b)); }
int ge(int a, int b) { return lor(ge(a, b), eq(a, b)); }

typedef struct { int quotient; int reminder; } DivRem;

DivRem div_rem(int a, int b) {
    DivRem res;
    res.quotient = 0;
    res.reminder = 0;

    if (eq(b, 0)) { return res; } else {}

    while (le(b, a)) {
        res.quotient = res.quotient + 1;
        a = a - b;
    }

    res.reminder = a;
    return res;
}

int mul(int a, int b) {
    int res = 0;
    while(a) {
        res = res + b;
        a = a - 1;
    }
    return res;
}

int factorial(int a) {
    if (eq(a, 0)) { return 1; } else {}

    return mul(a, factorial(a - 1));
}

void print_number_rec(int a) {
    if (eq(a, 0)) { return; } else {}

    DivRem dr = div_rem(a, 10);
    print_number_rec(dr.quotient);
    write('0' + dr.reminder);
}

void print_number(int a) {
    if (eq(a, 0)) { write('0'); return; } else {}
    print_number_rec(a);
}

int read_number() {
    int c = read();
    int res = 0;
    while (lnot(eq(c, '\n'))) {
        res = mul(res, 10) + (c - '0');
        c = read();
    }
    return res;
}

int main() {
    print_number(factorial(read_number()));
}
