#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int get_line_from_file(FILE *fp, char *buffer, size_t z) {
  int ch, strln;

  if (fp == NULL) {
    fp = stdin;
  }
  if (! (fgets(buffer, z, fp) == NULL)) {
    strln = strlen(buffer) - 1;
    if (buffer[strln] != '\n' && buffer[strln] != EOF) {
      do {
        ch = fgetc(fp);
      } while (ch != EOF && ch != '\n');
      return 2;
    }
    buffer[strln] = '\0';
    return 1;
  }
  return 0;
}
  
  

int main () {
  FILE *fptr;
  char line[200];
  char filename[100];
  int errno;
  
  printf("Enter filename:\n");
  get_line_from_file(stdin, filename, sizeof(filename));
  
  fptr = fopen(filename, "r");
  if (errno != 0) {
    perror("Error: ");
    return 1;
  }
  if (fptr == NULL) {
    printf("Error. Cannot open file %s.\n", filename);
    return 0;
  }
  while (1) {
    if (! get_line_from_file(fptr,line,sizeof(line)))
      return 0;
    
    printf("%s\n", line);
  }
  fclose(fptr);
  
  return 0;
}






