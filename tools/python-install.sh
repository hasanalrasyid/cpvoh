CXX="/usr/bin/g++"              \
./configure --prefix=/home/aku/opt/python3-3.8.2       \
            --enable-shared     \
            --with-system-expat \
            --with-system-ffi   \
            --with-ensurepip=yes --enable-loadable-sqlite-extensions --enable-optimizations &&
make -j4

