#!/bin/bash
cp emacs ~/.emacs

git clone git@github.com:elixir-lsp/elixir-ls.git ~/.emacs.d/elixir-ls
cd ~/.emacs.d/elixir-ls
mix deps.get
mix elixir_ls.release

git clone https://github.com/eclipse-jdtls/eclipse.jdt.ls.git ~/.emacs.d/jdtls
cd ~/.emacs.d/jdtls
./mvnw clean verify

