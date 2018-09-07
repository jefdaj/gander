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

echo "this is what \`diff -r\` says about them:"
diff -r backup current
echo

echo "and this is what \`gander diff\` says:"
gander diff backup current
echo

echo "duplicating the backup folder twice more..."
cp -r backup current/old-backup-1
cp -r backup current/old-backup-2
echo

echo "list all duplicates with \`gander dupes\`:"
cd ..
gander dupes demo
echo

echo "delete those two old-backup folders, then re-run:"
rm -rf demo/current/old-backup-*
gander dupes demo
echo
