gcc-6 -c gcc-src/gcc/tiger/tigerlib.c
gcc-install/bin/gcctiger -c $1 -o tiger.o
gcc-6 tiger.o tigerlib.o -o a.out
rm *.o
