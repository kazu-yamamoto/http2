#! /bin/sh

TOPDIR="test-frame"
CMD="./dist/build/frame-encode/frame-encode"

for i in $TOPDIR/source/*
do
  $CMD -w < $i > $TOPDIR/wire/`basename $i`
done

for i in $TOPDIR/wire/*
do
  $CMD < $i > $TOPDIR/json/`basename $i`
done
