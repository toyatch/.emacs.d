#!/bin/sh

emacs="emacs-27.2"

sudo yum update -y
sudo yum -y groupinstall "Development Tools"
sudo yum install -y gnutls-devel
sudo yum install -y ncurses-devel

curl https://src.fedoraproject.org/rpms/emacs/raw/f33/f/emacs.spec | \
    sed -e 's/m17n-lib-devel//g' -e 's/libotf-devel//g' | \
    grep ^BuildRequires | cut -d" " -f2 | xargs sudo dnf -y install

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
