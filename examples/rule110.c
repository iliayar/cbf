
int main() {
    int cur[100];
    int next[100];

    // cur[50 - 16 + 1] = 0;
    // cur[50 - 16 + 2] = 0;
    // cur[50 - 16 + 3] = 0;
    // cur[50 - 16 + 4] = 1;
    // cur[50 - 16 + 5] = 0;
    // cur[50 - 16 + 6] = 0;
    // cur[50 - 16 + 7] = 1;
    // cur[50 - 16 + 8] = 1;
    // cur[50 - 16 + 9] = 0;
    // cur[50 - 16 + 10] = 1;
    // cur[50 - 16 + 11] = 1;
    // cur[50 - 16 + 12] = 1;
    // cur[50 - 16 + 13] = 1;
    // cur[50 - 16 + 14] = 1;

    cur[100 - 12 + 1] = 0;
    cur[100 - 12 + 2] = 0;
    cur[100 - 12 + 2] = 0;
    cur[100 - 12 + 3] = 1;
    cur[100 - 12 + 4] = 1;
    cur[100 - 12 + 5] = 1;
    cur[100 - 12 + 6] = 0;
    cur[100 - 12 + 7] = 1;
    cur[100 - 12 + 8] = 1;
    cur[100 - 12 + 9] = 1;
    int iterations = 0;

    while (100 - iterations) {
        int i = 0;
        while (100 - i) {
            if (cur[i]) {
                write('#');
            } else {
                write('_');
            }
            i = i + 1;
        }
        write('\n');

        i = 1;
        while (100 - 1 - i) {
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
