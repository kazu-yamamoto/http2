#! /bin/sh

SRCDIR="test-hpack/hpack-test-case/node-http2-hpack"

for i in $SRCDIR/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-diff/`basename $i`
dist/build/hpack-encode/hpack-encode off diff "http2 in Haskell: Diff - index=yes, reference set=yes, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-diff-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on diff "http2 in Haskell: DiffH - index=yes, reference set=yes, huffman=yes" < $i > $dst
done

for i in $SRCDIR/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-linear/`basename $i`
dist/build/hpack-encode/hpack-encode off linear "http2 in Haskell: Linear - index=yes, reference set=no, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-linear-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on linear "http2 in Haskell: LinearH - index=yes, reference set=no, huffman=yes" < $i > $dst
done

for i in $SRCDIR/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-naive/`basename $i`
dist/build/hpack-encode/hpack-encode off naive "http2 in Haskell: Naive - index=no, reference set=no, huffman=no" < $i > $dst
done

for i in $SRCDIR/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-naive-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on naive "http2 in Haskell: NaiveH - index=no, reference set=no, huffman=yes" < $i > $dst
done
