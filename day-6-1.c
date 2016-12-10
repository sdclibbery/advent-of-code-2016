#include "stdio.h"
#include "stdlib.h"

#define charsPerLine 8
static const size_t bufLen = 1024;
typedef enum { StateRead, StateCount, StateFindBest } State;

void main (void) {
  char buf[bufLen];
//  FILE* pf = fopen("day-6.test.data", "r");
  FILE* pf = fopen("day-6.data", "r");
  char* line;
  int counts[128][charsPerLine] = {0,};
  int c, a;
  char best;
  int bestCount;
  State state = StateRead;
  int done = 0;

  do {
    if (state == StateRead) {
      line = fgets(buf, bufLen, pf);
      if (line == NULL) { done = 1; }
      c = 0;
      state = StateCount;
    } else if (state == StateCount) {
      counts[line[c]][c]++;
      c++;
      if (c >= charsPerLine) { state = StateRead; }
    }
  } while (!done); // The only allowed jump back for me; see http://number-none.com/blow/john_carmack_on_inlined_code.html

  for (c = 0; c < charsPerLine; c++) {
    best = '\0';
    bestCount = 1000000;
    for (a = 'a'; a <= 'z'; a++) {
      if (counts[a][c] < bestCount) {
        best = a;
        bestCount = counts[a][c];
      }
    }
    putchar(best);
  }
  puts("\n");

  fclose(pf);
}
