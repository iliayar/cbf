// Uncomment and clang rule110.c -o rule110 && ./rule110
// #include <stdio.h>
// void write(int c) {
//     printf("%c", (char)c);
// }

void print_array(int a[100]) {
    int SIZE = 100;

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

int main() {
    int SIZE = 100;
    int cur[100];
    int next[100];

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

        int i = 1;
        while (SIZE - 1 - i) {
            if (cur[i - 1]) {
                if (cur[i]) {
                    if (cur[i + 1]) {
                        next[i] = 0;
                    } else {
                        next[i] = 1;
                    }
                } else {
                    if (cur[i + 1]) {
                        next[i] = 1;
                    } else {
                        next[i] = 0;
                    }
                }
            } else {
                if (cur[i]) {
                    if (cur[i + 1]) {
                        next[i] = 1;
                    } else {
                        next[i] = 1;
                    }
                } else {
                    if (cur[i + 1]) {
                        next[i] = 1;
                    } else {
                        next[i] = 0;
                    }
                }
            }

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
