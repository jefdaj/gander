#!/usr/bin/env bash

rm -rf demo demo-hashes.txt
mkdir demo
cd demo

# The unicode test files are from:
# https://github.com/bits/UTF-8-Unicode-Test-Documents

echo -n "generating some horribly-named files..."
(for d in {1..100}; do
  unicodefile="$(ls ../unicode/* | shuf | head -n1)"
  baddir="$(cat "$unicodefile" | shuf | head -n1)"
  mkdir -p "$baddir"
  for f in {1..10}; do
    unicodefile="$(ls ../unicode/* | shuf | head -n1)"
    # badname="$(cat "$unicodefile" | shuf | head -n1 | cut -c-50)" # TODO the cut breaks gander??
    badname="$(cat "$unicodefile" | shuf | head -n1)"
    dst="${baddir}/${badname}"
    echo "this is the content of file $f" >> "$dst"
  done
done) 2>/dev/null
echo " done"

echo "the test dataset has $(find -type d | wc -l) folders with $(find -type f | wc -l) files"
echo "are only 10 unique files though"

echo -n "putting the folders randomly inside each other..."
SRC=(`ls | shuf`)
DST=(`ls | shuf`)
for n in {0..50}; do
  d=${DST[$n]}
  [[ -a $d ]] || continue
  for i in {1..10}; do
    s=${SRC[$((n + i))]}
    [[ -a $s ]] || continue
    [[ $s == $d ]] && continue
    mv $s $d
  done
done
echo " done"

cd ..

echo -n "hashing them with gander..."
gander hash demo > demo-hashes.txt
echo " done"
echo

echo "you can explore the demo folder and demo-hashes.txt to get an idea if you want"
echo -n "then press ENTER to look for duplicates"; read dummyvar
gander dupes demo-hashes.txt | less -R

# TODO annex init, move the dirs into unsorted, and annex add them
# TODO annex sort it back to just one dir with 10 files if possible
