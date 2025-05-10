// Uncomment and clang rule110.c -o rule110 && ./rule110
// #include <stdio.h>
// void write(int c) {
//     printf("%c", (char)c);
// }

void print_array(int a[50]) {
    int SIZE = 50;

    int i = 0;
    while (SIZE - i) {
        if (a[i]) {
            write('#');
        } else {
            write('_');
        }
        i = i + 1;
    }
    write('\n');
}

int rule110(int c0, int c1, int c2) {
    if (c0) {
        if (c1) {
            if (c2) { return 0; } 
            else { return 1; }
        } else {
            if (c2) { return 1; }
            else { return 0; }
        }
    } else {
        if (c1) {
            if (c2) { return 1; }
            else { return 1; }
        } else {
            if (c2) { return 1; }
            else { return 0; }
        }
    }
}

int main() {
    int SIZE = 50;
    int cur[50];
    int next[50];

    int i = 0;
    while (SIZE - i) {
        cur[i] = 0;
        next[i] = 0;
        i = i + 1;
    }

    cur[SIZE - 12 + 1] = 0;
    cur[SIZE - 12 + 2] = 0;
    cur[SIZE - 12 + 2] = 0;
    cur[SIZE - 12 + 3] = 1;
    cur[SIZE - 12 + 4] = 1;
    cur[SIZE - 12 + 5] = 1;
    cur[SIZE - 12 + 6] = 0;
    cur[SIZE - 12 + 7] = 1;
    cur[SIZE - 12 + 8] = 1;
    cur[SIZE - 12 + 9] = 1;
    int iterations = 0;

    while (SIZE - iterations) {
        print_array(cur);

        i = 1;
        while (SIZE - 1 - i) {
            next[i] = rule110(cur[i - 1], cur[i], cur[i + 1]);
            i = i + 1;
        }

        i = 0;
        while (SIZE - i) {
            cur[i] = next[i];
            i = i + 1;
        }
        iterations = iterations + 1;
    }
}
