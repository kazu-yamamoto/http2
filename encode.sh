#! /bin/sh

for i in test-hpack/hpack-test-case/nghttp2/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-diff/`basename $i`
dist/build/hpack-encode/hpack-encode off diff < $i > $dst
done

for i in test-hpack/hpack-test-case/nghttp2/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-diff-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on diff < $i > $dst
done

for i in test-hpack/hpack-test-case/nghttp2/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-linear/`basename $i`
dist/build/hpack-encode/hpack-encode off linear < $i > $dst
done

for i in test-hpack/hpack-test-case/nghttp2/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-linear-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on linear < $i > $dst
done

for i in test-hpack/hpack-test-case/nghttp2/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-naive/`basename $i`
dist/build/hpack-encode/hpack-encode off naive < $i > $dst
done

for i in test-hpack/hpack-test-case/nghttp2/*.json
do                                 
dst=test-hpack/hpack-test-case/haskell-http2-naive-huffman/`basename $i`
dist/build/hpack-encode/hpack-encode on naive < $i > $dst
done
