#! /bin/sh

SRCDIR="test-hpack/hpack-test-case/nghttp2"

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2-naive/`basename $i`
dist/build/hpack-encode/hpack-encode off naive "http2 in Haskell: Naive - static table=no, header table=no, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2-naive-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on naive "http2 in Haskell: NaiveH - static table=no, header table=no, huffman=yes" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2-static/`basename $i`
dist/build/hpack-encode/hpack-encode off static "http2 in Haskell: Static - static table=yes, header table=no, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2-static-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on static "http2 in Haskell: StaticH - static table=yes, header table=no, huffman=yes" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2-linear/`basename $i`
dist/build/hpack-encode/hpack-encode off linear "http2 in Haskell: Linear - static table=yes, header table=yes, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2-linear-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on linear "http2 in Haskell: LinearH - static table=yes, header table=yes, huffman=yes" < $i > $dst
done
