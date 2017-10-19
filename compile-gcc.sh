cd gcc-build
make -j$(getconf _NPROCESSORS_ONLN)
make install
../gcc-install/bin/gcctiger ../sintatico-f1-testes/test11.tig
