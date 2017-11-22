#define getchar _getchar
#define concat _concat
#define substring _substring
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#undef getchar
#undef concat
#undef substring

int *iniciaArray(int size, int init);
int *allocaRegistro(int size);
void printa(char *s);
void flush();
int ord(char *s);
char *chr(int i);
int size(char *s);
char *substring(char *s, int first, int n);
char *concat(char *a, char *b);
int negate(int i);
char *getchar();
