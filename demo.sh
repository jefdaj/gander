#!/usr/bin/env bash

rm -rf demo
mkdir -p demo/current/folder1/folder2
cd demo

echo "creating some files..."
echo "create a file"       > current/file1.txt
echo "create another file" > current/folder1/folder2/file2.txt
echo "and a third"         > current/folder1/file3.txt

echo "backing them up..."
cp -r current backup

echo "continuing to edit the originals..."
echo "edit the 2nd file"  >> current/folder1/folder2/file2.txt
echo "create a third file" > current/file3.txt
mv current/folder1/file3.txt current/folder1/folder2/

echo "ok, they look like:"
echo

cd ..; tree demo; cd demo
echo

echo "this is how \`diff -r\` explains the difference between backup and current:"
diff -r backup current
echo

echo "and this is how \`gander diff\` explains it:"
gander diff backup current
echo

echo "simpler for our purposes, right?"

echo "duplicating the backup folder twice more..."
cp -r backup current/old-backup-1
cp -r backup current/old-backup-2
echo

echo "you can list all duplicates with \`gander dupes\`:"
cd ..
gander dupes demo

echo "there are some overlapping duplicates, as usual."
echo "simplify it by deleting those two old-backup folders listed first,"
echo "then re-run to see what's still duplicated:"
rm -rf demo/current/old-backup-*
gander dupes demo
echo
