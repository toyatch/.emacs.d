#!/bin/sh

pkgmng="yum"
if [ $# -gt 0 -a $1 = "apt" ]; then
    pkgmng="apt"
fi

#### plantuml-mode
if [ $pkgmng = "apt" ]; then
    sudo apt install -y openjdk-11-jdk
    sudo apt install -y jre
    sudo apt install -y ipa-pgothic-fonts
    sudo apt install -y graphviz
else
    sudo yum install -y jre
    sudo yum install -y ipa-pgothic-fonts
    sudo yum install -y graphviz
fi

#### plantuml.jar
if [ ! -e plantuml.jar ]; then
    wget https://sourceforge.net/projects/plantuml/files/plantuml.jar
fi
sudo cp plantuml.jar ~/.emacs.d/
