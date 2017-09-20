cd gcc-build
rm -rf *
../gcc-src/configure --prefix=$(pwd)/../gcc-install --disable-bootstrap --enable-languages=c,c++,tiger
make -j$(getconf _NPROCESSORS_ONLN)
make install
