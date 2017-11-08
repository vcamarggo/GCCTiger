#include <stdio.h>

int *iniciaArray(int size, int init);
int *allocaRegistro(int size);
int stringIgual(struct string *s, struct string *t);
void printa(struct string *s);
void limpaOut();
int chartoint(struct string *s);
struct string *inttochar(int i);
int tamanho(struct string *s);
struct string *subpalavra(struct string *s, int first, int n);
struct string *concatena(struct string *a, struct string *b);
int not(int i);

#undef getchar

struct string *pegachar();
