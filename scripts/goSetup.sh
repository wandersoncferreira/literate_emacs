#!/usr/bin/env bash

echo 'install go complete daemon'
go get -u github.com/nsf/gocode

echo 'install go definitions'
go get -u github.com/rogpeppe/godef/...

echo 'install go imports'
go get -u golang.org/x/tools/cmd/goimports

echo 'install go tools'
go get -u golang.org/x/tools/cmd/...
go get -u golang.org/x/tools/cmd/guru

echo 'install go check'
go get -u github.com/dougm/goflymake

echo 'install go tags'
go get -u github.com/jstemmer/gotags
