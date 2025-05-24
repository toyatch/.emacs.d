#!/bin/sh

emacs="emacs-30.1"

sudo su -c "grep '^deb ' /etc/apt/sources.list | sed 's/^deb/deb-src/g' > /etc/apt/sources.list.d/deb-src.list"
sudo apt update -y
sudo apt install -y curl
sudo apt install -y build-essential
sudo apt build-dep -y emacs
sudo apt install -y libjansson-dev

if [ ! -e $emacs ]; then
  if [ ! -e $emacs.tar.gz ]; then
    curl -LO https://ftp.gnu.org/gnu/emacs/$emacs.tar.gz
  fi
  tar xf $emacs.tar.gz
fi

cd $emacs
./autogen.sh
./configure --without-x
make
sudo make install
