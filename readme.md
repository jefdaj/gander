gander
======

The "Git ANnex DedupER" deduplicates files, optionally using [git-annex][1] to track changes.

It's especially suited for cases where you have so many files that git-annex
alone slows to a crawl trying to index them all (several million +), where they
are too large to fit on your computer at once, or where you want to be really
sure not to delete anything important in the process.

Of course, it also handles small and medium-sized dedup jobs quickly.

Please send suggestions, pull requests, and/or report whether it worked for you.
Open to new use cases or syntax.


Install
-------

The project can be built with either [Stack][2] or [Nix][3]:

```
git clone https://github.com/jefdaj/gander.git
cd gander

# either one works. if unsure, use stack
stack build
nix-build
```

Email me or open an issue if you would like a precompiled binary release added.


Hash a folder
-------------

`gander` recursively hashes folders and uses the hashes for later comparison.

```
$ gander hash backup > backup-hashes.txt
$ cat backup-hashes.txt
e80d8ed374517e280f5923057046010b5786251e65dac402bf87b1a07f48780b file backup/file1.txt
cfc9494ec1483b639a5d07dcbfafb9b27048800d5ad6c1e320a36272c2e42880 file backup/folder1/file3.txt
8c0899afa99e1ea386150e72bd6b72e8e7ac78f5c0e984b97a0a10aa2982039b file backup/folder1/folder2/file2.txt
59563d46dcacd72056fc2f753929e54f6839d40f62f6ea82db42e556d5de2021 dir  backup/folder1/folder2
2e3f25ea48d7371a328d1356628f838bbb32352837cb3c26427eca4bab3e8060 dir  backup/folder1
711d2d4e20a5a30a6a52cdd7fc7c48f6805c5201cea860a8c1bcf72c9bb4398e dir  backup
```

The standalone hash command is mainly useful when the files to hash are large or on an external drive.


Diff two folders
----------------

You can compare two folders directly, or substitute the saved hashes.
These do the same thing:

```
gander diff backup            current
gander diff backup-hashes.txt current
```

Output is like `diff -r`, but simplified by assuming the first folder is older.
[demo.sh][4] compares them:

```
creating some demo files...
backing them up...
continuing to edit the originals...
ok, they look like:

demo
├── backup
│   ├── file1.txt
│   └── folder1
│       ├── file3.txt
│       └── folder2
│           └── file2.txt
└── current
    ├── file1.txt
    ├── file3.txt
    └── folder1
        └── folder2
            ├── file2.txt
            └── file3.txt

6 directories, 7 files

this is what `diff -r` says about them:
Only in current: file3.txt
Only in backup/folder1: file3.txt
diff -r backup/folder1/folder2/file2.txt current/folder1/folder2/file2.txt
1a2
> edit the 2nd file
Only in current/folder1/folder2: file3.txt

and this is what `gander diff` says:
added 'file3.txt' (03c6a1ef)
moved 'folder1/file3.txt' -> 'folder1/folder2/file3.txt' (cfc9494e)
edited 'folder1/folder2/file2.txt/file2.txt' (8c0899af -> a86b253f)
```

The hex codes in parentheses are the first 8 characters of the relevant hashes.
Individual file hashes are the same as reported by `sha256sum`.
Everything works similarly with directories and binary files,
except that the directory hashes don't follow an external standard.


Find dupes within a folder
--------------------------

What if you have a more complicated mess of files to deduplicate?
Say we made a couple more copies of the `backup` demo folder "just in case", then forgot about it.
Continuing with [demo.sh][4],
`gander` can group all the identical files/folders and sort them by number of files:

```
$ gander dupes demo
# 3 duplicate dirs with 3 files each (9 total)
demo/backup
demo/current/old-backup-1
demo/current/old-backup-2

# 4 duplicate files
demo/backup/folder1/file3.txt
demo/current/folder1/folder2/file3.txt
demo/current/old-backup-1/folder1/file3.txt
demo/current/old-backup-2/folder1/file3.txt

# 4 duplicate files
demo/backup/file1.txt
demo/current/file1.txt
demo/current/old-backup-1/file1.txt
demo/current/old-backup-2/file1.txt
```

It still looks messy because some of the duplicate sets overlap,
but if we focus on the top set we can see that it correctly picked out the overall problem.

So we delete `current/old-backup-1` and `current/old-backup-2` and re-run it:

```
$ gander dupes demo
# 2 duplicate files
demo/backup/folder1/file3.txt
demo/current/folder1/folder2/file3.txt

# 2 duplicate files
demo/backup/file1.txt
demo/current/file1.txt
```

Much better! Even extremely large, messy drives can be simplified after several rounds.


Annex-aware mode
----------------

`gander` was really designed to automate the above "find dupes -> delete dupes
-> update hashes" loop for gradual deduplication of large messes. The goals are:

1. Be sure nothing is deleted or lost accidentally
2. Be clear about what it will do at each step for non-programmers
3. Be reasonably fast, given the limitations of git

It was written for a specific project consisting of around 10 million files.
That project included complete Mac filesystems and some really horrendous
filenames including emojis, newlines, and unicode glyphs. So it made a decent
stress test.

There are two parts to the overall cleanup strategy:

1. Add everything to a cental [git-annex][1] repository and let git-annex
   deduplicate the file contents immediately by symlinking duplicate files to the
   same store path. That leaves a large number of directories + duplicate
   symlinks, but takes up minimal disk space. It can be done without `gander`.

2. Use `gander` to iteratively deduplicate the directories and symlinks. It
   double-checks that each step is safe (no unique files lost), and even if you
   make a mistake the full history is still in git. It also avoids re-hashing
   files by reading their `sha256sum`s from the git-annex symlinks.

Once satisfied that the project went well you can either keep your deduplicated
files in git-annex or `git annex unannex` them.

To be extra careful, you can also re-hash external folders later to confirm
that everything from them made it into the annex. Or re-hash the annex itself
to check file integrity with `git annex fsck`.


[1]: https://git-annex.branchable.com
[2]: https://docs.haskellstack.org/en/stable/README/
[3]: https://nixos.org/nix
[4]: demo.sh
