#! /usr/bin/sh

runghc -- -no-user-package-db -package-db --ghc-arg=.cabal-sandbox/x86_64-osx-ghc-7.6.3-packages.conf.d -itest-hpack test-hpack/encode.hs
