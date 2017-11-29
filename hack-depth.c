#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
 *  gcc -Wall -O2 -std=c99 -o hack-depth hack-depth.c
*/

// Usage: samtools view bamFile.bam | hack-depth

#define CONTIGLEN 1000000
// 5, to cover 4 bases + N
#define NBASES 5
int depth[CONTIGLEN][NBASES];
int max_loc = 0;

char dnaChar[] = "ATGCN";
int char2int(char c) {
  switch(c) {
  case 'A': return 0;
  case 'T': return 1;
  case 'G': return 2;
  case 'C': return 3;
  case 'N': return 4;
  }
  exit(-1);
}

int main() 
{
  int lineLen=1024;
  char buf[lineLen];
  while(fgets(buf, lineLen, stdin)) {
    char *p = strtok(buf, "\t");
    int i=1;
    char *chr,*cigar,*seq;
    int loc, flags;
    while ((p = strtok(NULL, "\t"))) {
      if (i==1) flags=atoi(p);
      if (i==2) chr=p;
      if (i==3) loc=atoi(p);
      if (i==5) cigar=p;
      if (i==9) seq=p;
      if (i>9)
        break;
      i++;
    }

    if (strcmp(cigar,"*")==0)     // unmapped
      continue;

    if (flags & 0x800)  // Supplementary alignment.  Skip it
      continue;

    // Parse CIGAR
    while (*cigar!=0) {
      int n=atoi(cigar);
      while (isdigit(*cigar)) cigar++;
      char c=*(cigar++);
      switch(c) {
      case 'S':
      case 'I': 
          while((n--)>0) seq++;
          break;
      case 'D': 
      case 'N': 
          while((n--)>0) loc++;
          break;
      case 'M':
          while ((n--)>0) {
            depth[loc][char2int(*seq)]++;
            loc++;
            seq++;
            max_loc = loc > max_loc ? loc : max_loc;
          }
          break;
      case 'H':
          break;
      default:
        fprintf(stderr, "unknown cigar : %c\n",c);
        exit(-1);
      }
    }
  }
  // print header
  printf("loc\t");
  for (int j=0; j<NBASES; j++) {
      printf("%c\t", dnaChar[j]);
  }
  printf("type\n");
  for (int i=0; i<max_loc; i++) {
    printf("%d\t",i);
    int total=0;
    int max=0;
    for (int j=0; j<NBASES; j++) {
      printf("%d\t", depth[i][j]);
      total += depth[i][j];
      max = max>depth[i][j] ? max : depth[i][j];
    }
    if (total>0 && 1.0 * max/total < 0.9) {
      printf("HET");
    } else {
      printf("HOMO");
    }
    printf("\n");
  }
}
