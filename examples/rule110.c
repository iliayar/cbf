void print_array(int a[200]) {
    int SIZE = 200;

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
    int SIZE = 200;
    int cur[200];
    int next[200];

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

        cur = next;
        iterations = iterations + 1;
    }
}
