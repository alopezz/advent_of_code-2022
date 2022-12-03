#include <stdlib.h>
#include <stdio.h>


void update_calories(int top_calories[3], int new_value) {
     for (int idx = 2; idx >= 0; idx--) {
          if (new_value > top_calories[idx]) {
               if (idx < 2) {
                    /* Displace existing value if not last in top */
                    top_calories[idx+1] = top_calories[idx];
               }
               top_calories[idx] = new_value;
          } else {
               break;
          }
     }
}


int main() {
     int top_calories[3] = {0, 0, 0};
     int curr_calories = 0;

     char buffer[256];
     while (fgets(buffer, sizeof buffer, stdin) != NULL) {
          if (buffer[0] == '\n') {
               update_calories(top_calories, curr_calories);
               curr_calories = 0;
          } else {
               curr_calories += atoi(buffer);
          }
     }

     int max_calories = top_calories[0];

     printf("Part 1 solution: %d\n", max_calories);
     printf("Part 2 solution: %d\n", top_calories[0] + top_calories[1] + top_calories[2]);
  
     return 0;
}
