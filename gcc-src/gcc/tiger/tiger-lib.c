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
void printFloat(float f);
void flush();
int ord(char *s);
char *chr(int i);
int size(char *s);
char *substring(char *s, int first, int n);
char *concat(char *a, char *b);
int negate(int i);
char *getchar();

void print(char *s) {
	printf("%s", s);
}

void printInt(int i) {
	printf("%d", i);
}

void printReal(float f) {
	printf("%f", f);
}

void flush() {
	fflush(stdout);
}


int ord(char *s) {
	if(s== NULL || strlen(s) == 0)
		return -1;
	else
		return s[0];
}

char *chr(int i) {
	if (i<0 || i>=256) 
		{printf("chr(%d) out of range\n",i); exit(1);}
	char *str;
	str = (char *) malloc (sizeof(char)*2);
	str[0] = i;
	str[1] = '\0';
	return str;
}

int size(char *s) { 
	return strlen(s);
}

char *substring(char *s, int first, int n) {
	if (first > strlen(s)  -1) 
	{printf("ERROR: first out of range\n"); exit(1);}

	if (first + n > strlen(s)) 
	{printf("ERROR: On out of range\n"); exit(1);}

	char *str = (char *) malloc (sizeof(char)*(n+1));
	for (int i =0; i < n ; i++){
	  str[i] = s[first+i];
	}
	str[n] = '\0';
	return str;
}

char *concat(char *a, char *b) {
	char *str = (char *) malloc (sizeof(char) * (strlen(a) + strlen(b)));
	strcat(str,a);
	strcat(str,b);
	return str;
}

int negate(int i) {
	return !i;
}

char *getchar() {
	char *str;
	str = (char *) malloc (sizeof(char)*2);
	scanf( "%1s",str);
	str[1] = '\0';
	return str;
}

