#include <stdio.h>
#include <stdlib.h>

int *iniciaArray(int size, int init);
int *allocaRegistro(int size);
int stringIgual(char *s, char *t);
void printa(char *s);
void limpaOut();
int chartoint(char *s);
char *inttochar(int i);
int tamanho(char *s);
char *subpalavra(char *s, int first, int n);
char *concatena(char *a, char *b);
int nao(int i);
char *pegachar();