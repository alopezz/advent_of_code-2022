#include <stdio.h>

int main() {
     int total = 0;
     int total2 = 0;

     /* Read line by line */
     char buffer[256];
     while (fgets(buffer, sizeof buffer, stdin) != NULL) {
          int a_start, a_end, b_start, b_end;
          sscanf(buffer, "%d-%d,%d-%d", &a_start, &a_end, &b_start, &b_end);
          /* Full containment (part 1) */
          if (a_start >= b_start && a_end <= b_end || b_start >= a_start && b_end <= a_end) {
               total += 1;
          }

          /* Full containment (part 2) */
          if (
               a_start >= b_start && a_start <= b_end ||
               b_start >= a_start && b_start <= a_end ||
               a_end >= b_start && a_end <= b_end ||
               b_end >= a_start && b_end <= a_end
               ) {
               total2 += 1;
          }
     }

     printf("Part 1 solution: %d\n", total);
     printf("Part 2 solution: %d\n", total2);
}
