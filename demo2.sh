#!/usr/bin/env bash

rm -rf demo
mkdir demo
pushd demo

# create 100 dirs with 10 files each
for d in {1..100}; do
  mkdir dir${d}
  for f in {1..10}; do
    echo "this is file number ${f}" >> dir${d}/file${f}.txt
  done
done

# put them randomly inside each other
SRC=(`ls | shuf`)
DST=(`ls | shuf`)
for n in {0..50}; do
  d=${DST[$n]}
  [[ -a $d ]] || continue
  for i in {1..3}; do
    s=${SRC[$((n + i))]}
    [[ -a $s ]] || continue
    [[ $s == d ]] && continue
    mv $s $d
  done
done

# list dupes
# should group dirs by tree structure (one dir with two others in it etc)
# and there should be 10 groups of 100 duplicate files each
popd
gander dupes demo | less -R

# TODO annex init, move the dirs into unsorted, and annex add them
# TODO annex sort it back to just one dir with 10 files if possible
