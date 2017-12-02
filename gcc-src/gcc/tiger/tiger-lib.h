#define getchar _getchar
#define concat _concat
#define substring _substring
#define print _print
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#undef getchar
#undef concat
#undef substring
#undef print

void print(char *s);
void printInt(int i);
void printReal(float f);
void flush();
int ord(char *s);
char *chr(int i);
int size(char *s);
char *substring(char *s, int first, int n);
char *concat(char *a, char *b);
int negate(int i);
char *getchar();
