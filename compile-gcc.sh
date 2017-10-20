cd gcc-build
make -j$(getconf _NPROCESSORS_ONLN)
make install
