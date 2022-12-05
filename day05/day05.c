#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define STACK_MAX_SIZE 1000

/* Rather than implement a fully dynamic stack, we simplify by using a
 * fixed size backing array and use a pointer. */
typedef struct {
     char arr[STACK_MAX_SIZE];
     int pointer;
} stack;


/* Create a new empty stack */
stack new_stack() {
     stack s;
     s.pointer = 0;
     return s;
}

/* Push an element into stack */
void stack_push(stack* s, char c) {
     s->arr[s->pointer] = c;
     s->pointer++;
}

/* Pop an element from a stack */
char stack_pop(stack* s) {
     char rv = s->arr[s->pointer - 1];
     s->pointer--;
     return rv;
}

/* Return a reversed version of the stack */
stack stack_reversed(stack* s) {
     stack rev;
     rev.pointer = s->pointer;
     for (int i = 0; i < s->pointer; i++) {
          rev.arr[s->pointer - i - 1] = s->arr[i];
     }

     return rev;
}

/* Move a crate from src to dst */
void move_crate(stack* stacks, int count, int src, int dst) {
     for (int i = 0; i < count; i++) {
          stack_push(&stacks[dst], stack_pop(&stacks[src]));
     }
}

void move_crate_9001(stack* stacks, int count, int src, int dst) {
     stack temp = new_stack();
     for (int i = 0; i < count; i++) {
          stack_push(&temp, stack_pop(&stacks[src]));
     }

     for (int i = 0; i < count; i++) {
          stack_push(&stacks[dst], stack_pop(&temp));
     }
}


int main() {
     int n_stacks = 0;
     stack* stacks_1;
     stack* stacks_2;

     /* Read line by line */
     char buffer[256];
     while (fgets(buffer, sizeof buffer, stdin) != NULL) {
          /* We exit when the line with labels for each stack */
          if (buffer[1] == '1') {
               break;
          }

          /* Inputs are zero-padded to the same length as necessary, so
             checking the length of just the first line is fine. */
          if (n_stacks == 0) {
               n_stacks = (strlen(buffer) + 1) / 4;
               /* Initialize stacks */
               stacks_1 = calloc(n_stacks, sizeof(stack));
               stacks_2 = calloc(n_stacks, sizeof(stack));
               for (int i = 0; i < n_stacks; i++) {
                    stacks_1[i] = new_stack();
               }
          }

          for (int i = 0; i < n_stacks; i++) {
               char c = buffer[4*i + 1];
               if (c != ' ') {
                    stack_push(&stacks_1[i], c);
               }
          }
     }

     for (int i = 0; i < n_stacks; i++) {
          stacks_1[i] = stack_reversed(&stacks_1[i]);
     }
     // Independent copy for part 2 (CrateMover 9001)
     for (int i = 0; i < n_stacks; i++) {
          stacks_2[i] = stacks_1[i];
     }

     /* Read and follow the instructions */
     while (fgets(buffer, sizeof buffer, stdin) != NULL) {
          int count, src, dst;

          /* Parse instruction */
          if (sscanf(buffer, "move %d from %d to %d", &count, &src, &dst) < 3) {
               /* Skip unparseable lines */
               continue;
          }

          move_crate(stacks_1, count, src - 1, dst - 1);
          move_crate_9001(stacks_2, count, src - 1, dst - 1);
     }

     /* Read final state */
     char message[256];

     int idx = 0;
     for (idx = 0; idx < n_stacks; idx++) {
          message[idx] = stack_pop(&stacks_1[idx]);
     }
     message[idx] = 0;
     printf("Result for part 1 is: \"%s\"\n", message);

     for (idx = 0; idx < n_stacks; idx++) {
          message[idx] = stack_pop(&stacks_2[idx]);
     }
     message[idx] = 0;
     printf("Result for part 2 is: \"%s\"\n", message);

     /* Free stuff */
     free(stacks_1);
     free(stacks_2);
}
