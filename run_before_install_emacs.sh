#!/bin/bash

git clone git@github.com:elixir-lsp/elixir-ls.git ~/.emacs.d/elixir-ls
cd ~/.emacs.d/elixir-ls
mix deps.get
mix elixir_ls.release

git clone https://github.com/eclipse-jdtls/eclipse.jdt.ls.git ~/.emacs.d/jdtls
wget -c https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.9.0/jdt-language-server-1.9.0-202203031534.tar.gz -O /tmp/jdtls.tar.gz
rm -rf ~/.emacs.d/jdtls
mkdir ~/.emacs.d/jdtls
tar -xvf /tmp/jdtls.tar.gz --directory ~/.emacs.d/jdtls

