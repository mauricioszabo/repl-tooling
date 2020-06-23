#!/bin/bash

sudo apt-get install wget unzip
wget https://github.com/borkdude/babashka/releases/download/v0.1.2/babashka-0.1.2-linux-static-amd64.zip
unzip babashka-0.1.2-linux-static-amd64.zip
sudo mv bb /usr/bin
./scripts/deploy.bb
