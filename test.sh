#!/usr/bin/env bash

rm -rf test
mkdir -p test
cd test

mkdir -p dir1/a/b/c
echo "some text yo" > dir1/a/b/file1.txt
echo "some text yo" > dir1/a/b/c/file2.txt
gander hash dir1 > dir1.txt

mkdir -p dir2/d/e/f/g/h
echo "more text yo" > dir2/d/file3.txt
echo "more text yo" > dir2/d/e/f/g/h/file4.txt
gander hash dir2 > dir2.txt

mv dir2 dir1/a/b
gander hash dir1 > moved.txt

gander update dir1.txt dir2.txt "a/b" > updated.txt
diff moved.txt updated.txt && echo "hooray it works!"
