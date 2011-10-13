#!/bin/sh

# This script will remove all traces of Haskell and GHC and Cabal from your debian
# system and install ghc-7.0.3 and the Haskell Platform. Then it will install
# Pugs. Good luck abd patience. You'll need both in spades.
#
# Usage:
#   PLATFORM=i386 ./install-haskell-platform-ghc7-and-pugs-on-debian-or-ubuntu-from-scratch.sh
# 
# Where PLATFORM is one of the values below:
#
# PLATFORM=i386
# PLATFORM=x86_64

if [ -z "$PLATFORM" ]; then
    echo "You need to set PLATFORM environment variable to i386 or x86_64"
    exit 1
fi

# Start with a Haskell-free system:
sudo apt-get purge haskell-platform ghc
sudo apt-get autoremove
rm -fr ~/.cabal

# Install debian deps for haskell-platform
sudo apt-get install libgmp3-dev libedit2 libedit-dev freeglut3-dev libglu1-mesa-dev
# Install debian deps for pugs
sudo apt-get install libperl-dev libncurses-dev libncurses5-dev

VERSION=7.0.3
NAME=ghc-$VERSION
TARBALL=$NAME-$PLATFORM-unknown-linux.tar.bz2
rm -fr $NAME
# Download is huge so we do this in case we need to repeat:
if [ ! -f ~/$TARBALL ]; then
    wget http://haskell.org/ghc/dist/$VERSION/$TARBALL
    mv $TARBALL ~/$TARBALL
fi
tar xjf ~/$TARBALL

cd $NAME
./configure
sudo make install
cd -
rm -fr $NAME

VERSION=2011.2.0.1
NAME=haskell-platform-$VERSION
TARBALL=$NAME.tar.gz

rm -fr $NAME $TARBALL
wget http://lambda.galois.com/hp-tmp/$VERSION/$TARBALL
tar xzf $TARBALL
cd $NAME
./configure
make
sudo make install
cd -
rm -fr $NAME $TARBALL

# Time to install Pugs!
cabal upgrade
cabal install pugs

echo Haskell and Pugs installed!
echo
echo 'Try: pugs -e '"'"'say "Pugs works!"'"'"
