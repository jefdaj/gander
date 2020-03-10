#! /bin/bash
# based on https://unix.stackexchange.com/a/199865

# set -x

rm -rf random-files
mkdir random-files
cd random-files

echo -n "generating 100 random files..."
for n in {1..100}; do
  dd if=/dev/urandom of=file$( printf %03d "$n" ).bin bs=1 count=$(( RANDOM + 1024 )) 2>/dev/null
done
echo " ok"
sleep 1

echo -n "duplicating them into 20 random folders..."
seq 1 20 | shuf | while read n; do
  dir="dir${n}"
  mkdir $dir
  ls | shuf | head | while read f; do
    cp -r "$f" "$dir" 2>/dev/null
  done
done
echo " ok"
sleep 1

cd ..
echo "they look like this:"
sleep 1
tree random-files | less

echo -n "hashing everything..."
gander hash random-files > random-files-hashes.txt
echo " ok"
sleep 1

echo "hashes look like this:"
less random-files-hashes.txt
sleep 1

echo
echo "ok, now we'll list duplicates."
echo "if done correctly, all the dupes should share the same basenames."
echo "(of course, this isn't true in general!)"
echo "press enter to continue"
read dummy

echo -n "listing duplicates..."
gander dupes random-files-hashes.txt | less
echo " ok"
