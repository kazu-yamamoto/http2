#! /bin/sh

SRCDIR="test-hpack/hpack-test-case/nghttp2/nghttp2-default"

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2/haskell-http2-diff/`basename $i`
dist/build/hpack-encode/hpack-encode off diff "http2 in Haskell: Diff - index=yes, reference set=yes, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2/haskell-http2-diff-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on diff "http2 in Haskell: DiffH - index=yes, reference set=yes, huffman=yes" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2/haskell-http2-linear/`basename $i`
dist/build/hpack-encode/hpack-encode off linear "http2 in Haskell: Linear - index=no, reference set=no, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2/haskell-http2-linear-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on linear "http2 in Haskell: LinearH - index=no, reference set=no, huffman=yes" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2/haskell-http2-static/`basename $i`
dist/build/hpack-encode/hpack-encode off static "http2 in Haskell: Static - index=yes, reference set=no, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2/haskell-http2-static-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on static "http2 in Haskell: StaticH - index=yes, reference set=no, huffman=yes" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2/haskell-http2-naive/`basename $i`
dist/build/hpack-encode/hpack-encode off naive "http2 in Haskell: Naive - index=no, reference set=no, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do
dst=test-hpack/hpack-test-case/haskell-http2/haskell-http2-naive-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on naive "http2 in Haskell: NaiveH - index=no, reference set=no, huffman=yes" < $i > $dst
done
