#!/bin/bash -e
if ghc-pkg describe qt >/dev/null; then
    exit
fi
cabal install OpenGL
if [[ ! -e qt-1.1.6.1 ]]; then
    cabal unpack qt-1.1.6.1
    patch -p1 -iqt-1.1.6.1.patch
fi
(
    cd qt-1.1.6.1
    bash build --no-configure --no-build --no-install --no-samples
    cabal install --extra-include-dirs=$PWD/qws/include --extra-lib-dirs=$PWD/qws/bin
)
