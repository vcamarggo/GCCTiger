#include <stdio.h>
#include <stdlib.h>

int *iniciaArray(int size, int init);
int *allocaRegistro(int size);
int stringIgual(char *s, char *t);
void printa(char *s);
void limpaOut();
int chartoint(char *s);
struct string *inttochar(int i);
int tamanho(char *s);
struct string *subpalavra(struct string *s, int first, int n);
struct string *concatena(struct string *a, struct string *b);
int not(int i);

#undef getchar

char *pegachar();
