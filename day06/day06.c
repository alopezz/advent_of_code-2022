#include <stdlib.h>
#include <stdio.h>

#define SET_SIZE 5
#define PACKET_SIZE 4
#define MESSAGE_SIZE 14


typedef struct {
     int *arr;
     int pointer;
     int size;
} Buffer;


Buffer make_buffer(size_t s) {
     Buffer b = {0};
     b.arr = calloc(1, s*sizeof(int));
     b.size = s;
     b.pointer = 0;
     return b;
}


void buffer_add(Buffer *b, int c) {
     b->arr[b->pointer] = c;
     b->pointer = div(b->pointer + 1, b->size).rem;
}


int buffer_all_different(Buffer *b) {
     /* Given that all characters contain lowercase ascii, we can use
        a small bitset to keep track of used characters. */

     char set[SET_SIZE] = {0};
     for (int idx = 0; idx < b->size; idx++) {
          char bit = b->arr[div(b->pointer + idx, b->size).rem] - 'a';
          /* Check if the bit is already set, which means we're repeating */
          if (set[div(bit, 8).quot] & (1 << div(bit, 8).rem)) {
               return 0;
          }
          /* Otherwise, set the bit for the current character */
          set[div(bit, 8).quot] |= (1 << div(bit, 8).rem);
     }

     return 1;
}


int main() {
     int c;
     int counter = 0;
     int result1 = 0;
     int result2 = 0;
     Buffer buf1 = make_buffer(PACKET_SIZE);
     Buffer buf2 = make_buffer(MESSAGE_SIZE);
     while ((c = fgetc(stdin)) != EOF) {
          buffer_add(&buf1, c);
          buffer_add(&buf2, c);
          counter++;

          if (counter >= PACKET_SIZE && result1 == 0) {
               if (buffer_all_different(&buf1)) {
                    result1 = counter;
               }
          }

          if (counter >= MESSAGE_SIZE && result2 == 0) {
               if (buffer_all_different(&buf2)) {
                    result2 = counter;
               }
          }

          /* Exit when we're done */
          if (result1 && result2) {
               break;
          }
     }

     printf("Part 1 solution: %d\n", result1);
     printf("Part 2 solution: %d\n", result2);
     return 0;
}
