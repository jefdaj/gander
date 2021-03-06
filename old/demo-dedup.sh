#!/usr/bin/env bash

checkdep() {
  which $1 &> /dev/null || (echo "you need $1 to run the demo"; exit 1)
}

linktree() {
  tree "$1" | cut -d'>' -f1 | sed 's/-$//g'
}

checkdep git-annex
checkdep gander

rm -rf demo demo-hashes.txt
mkdir -p demo/current/folder1/folder2
cd demo

echo "creating some files..."
echo "create a file"       > current/file1.txt
echo "create another file" > current/folder1/folder2/file2.txt
echo "and a third"         > current/folder1/file3.txt

echo "backing them up..."
cp -r current backup
cp -r backup current/old-backup-1
cp -r backup current/old-backup-2

echo "continuing to edit the originals..."
echo "edit the 2nd file"  >> current/folder1/folder2/file2.txt
echo "create a third file" > current/file3.txt
mv current/folder1/file3.txt current/folder1/folder2/
# TODO shouldn't need the PWD here! fix path handling

echo -n "creating the myfirstdedup git-annex repo..."
gander $PWD/myfirstdedup init # -v
# tail $PWD/myfirstdedup/hashes.txt
echo "done"

echo "copying the files to it and hashing them..."

# paths error introduced here:
gander $PWD/myfirstdedup add backup  ./backup # -v

# tail $PWD/myfirstdedup/hashes.txt
# exit
gander $PWD/myfirstdedup add current ./current # -v
# tail $PWD/myfirstdedup/hashes.txt
echo "done"
echo

echo "ok, the repo looks like:"
linktree myfirstdedup
echo

echo "press ENTER to dedup the annexed files"; read dummyvar
gander $PWD/myfirstdedup dedup # -v
# tail $PWD/myfirstdedup/hashes.txt
echo

echo "notice that gander left the unique files where they were:"
linktree myfirstdedup
echo

echo "you still have to sort those yourself"
