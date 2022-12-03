#include <stdlib.h>
#include <stdio.h>


int score_our_hand(char hand) {
     return hand - 'X' + 1;
}


int main() {
     int total = 0;
     int total2 = 0;

     char buffer[5];
     while (fgets(buffer, sizeof buffer, stdin) != NULL) {
          /* 0 if we lose, 3 if we draw, 6 if we win */
          total += 3 * div(3 + (buffer[2] - 'X' + 1) - (buffer[0] - 'A'), 3).rem;
          /* In part 2, we already have that value */
          total2 += 3*(buffer[2] - 'X');
          /* Decide our hand based on result for part 2 */
          char our_hand = 'X' + div(3 + buffer[0] - 'A' + buffer[2] - 'Y', 3).rem;

          /* Score intrinsic to our hand shape */
          total += score_our_hand(buffer[2]);
          total2 += score_our_hand(our_hand);
     }

     printf("Part 1 solution: %d\n", total);
     printf("Part 2 solution: %d\n", total2);
}
