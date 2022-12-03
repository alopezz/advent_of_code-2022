#include <stdlib.h>
#include <stdio.h>
#include <string.h>


int prio(char c) {
     if (c >= 'a' && c <= 'z') {
          return c - 'a' + 1;
     }
     if (c >= 'A' && c <= 'Z') {
          return c - 'A' + 27;
     }
     return 0;
}

void fill_set(char set[7]) {
     for (int n = 0; n < 7; n++) {
          set[n] = 0xff;
     }
}

void make_set(char set[7], char rucksack[256]) {
     int len = strlen(rucksack) - 1;
     for (int idx = 0; idx < len; idx++) {
          char bit_n = prio(rucksack[idx]) - 1;
          set[div(bit_n, 8).quot] |= 1 << (div(bit_n, 8).rem);
     }
}

void intersect(char a[7], char b[7]) {
     for (int n = 0; n < 7; n++) {
          a[n] &= b[n];
     }
}

int set_contains(char set[7], char bit) {
     return set[div(bit, 8).quot] & (1 << div(bit, 8).rem);
}

int score_part1(char buffer[256]) {
     char set[7] = {0};
     int len = strlen(buffer) - 1;
     for (int idx = 0; idx < len / 2; idx++) {
          char bit_n = prio(buffer[idx]) - 1;
          set[div(bit_n, 8).quot] |= 1 << (div(bit_n, 8).rem);
     }

     for (int idx = len/2; idx < len; idx++) {
          char bit_n = prio(buffer[idx]) - 1;
          if (set_contains(set, bit_n)) {
               return bit_n + 1;
          }
     }

     return 0;
}

int score_part2(char set[7], int *count, char buffer[256]) {
     /* Set for current rucksack */
     char current_set[7] = {0};
     make_set(current_set, buffer);

     /* Intersection with accumulated set */
     intersect(set, current_set);

     *count += 1;

     /* After three lines, we find the only bit that's set */
     if (div(*count, 3).rem == 0) {
          for (char n = 0; n < 52; n++) {
               if (set_contains(set, n)) {
                    fill_set(set);
                    return n + 1;
               }
          }
     }
     return 0;
}

int main() {
     int total = 0;
     int total2 = 0;

     /* Variables to keep track of things on part 2 */
     int count = 0;
     char set[7];
     fill_set(set);

     char buffer[256];
     while (fgets(buffer, sizeof buffer, stdin) != NULL) {
          total += score_part1(buffer);
          total2 += score_part2(set, &count, buffer);
     }

     printf("Part 1 solution: %d\n", total);
     printf("Part 2 solution: %d\n", total2);
}
