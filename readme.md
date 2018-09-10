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
File hashes are `sha256sum`s. The `F` or `D` and number before each one is for
recreating the tree structure: "this is a file at indent level 1", etc.

```
$ gander hash backup > backup-hashes.txt
$ cat backup-hashes.txt
F 1 e80d8ed374517e280f5923057046010b5786251e65dac402bf87b1a07f48780b file1.txt
F 2 cfc9494ec1483b639a5d07dcbfafb9b27048800d5ad6c1e320a36272c2e42880 file3.txt
F 3 8c0899afa99e1ea386150e72bd6b72e8e7ac78f5c0e984b97a0a10aa2982039b file2.txt
D 2 58cf38d7b7f029c489e2f5abbff6098270758a4d91a785884d7c5400a1b1dafb folder2
D 1 3081a6d2b5750111a1f7e411921327b356e15605a8e44515f05517f3e168f76d folder1
D 0 0f81ad31a218bd14a34ac490651e1b785ea93096219d12eb23c3b77b9337784a backup
```

The standalone hash command is mainly useful when the files to hash are large or on an external drive.


Diff two folders
----------------

You can diff two folders directly, or substitute the saved hashes.
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

Everything works similarly with directories and binary files,
except that the directory hashes don't follow an external standard.


Find dupes within a folder
--------------------------

What if you have a more complicated mess of files to deduplicate?
Say we made a couple more copies of the `backup` demo folder "just in case", then forgot about it.
Continuing with [demo.sh][4],
`gander` can group the identical files/folders and sort them by number of files:

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

It still looks messy because the duplicate sets overlap,
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

Much better! Even very large, messy drives can be simplified after several rounds.


Annex-aware mode
----------------

`gander` was really designed to automate the above "hash files -> find dupes ->
delete dupes -> update hashes" loop for gradual deduplication of large messes.
The goals are:

1. Be sure nothing is deleted or lost accidentally
2. Be clear about what it will do at each step for non-programmers
3. Be reasonably fast, given the limitations of git

It was written for a specific project consisting of around 10 million files.
That project included complete Mac filesystems and some really horrendous
filenames including emojis, newlines, and unicode glyphs. So it made a decent
stress test.

There are two parts to the overall cleanup strategy:

1. Add everything to a cental [git-annex][1] repository and let git-annex
   deduplicate the file contents immediately. That leaves a large number of
   directories + duplicate symlinks, but takes up minimal disk space. It can be
   done without `gander`.

2. Use `gander` to iteratively deduplicate the directories and symlinks. It
   double-checks that each step is safe (no unique files lost), and even if you
   make a mistake the full history is still in git. It also avoids re-hashing
   files by reading their `sha256sum`s from the git-annex symlinks.

Once satisfied that the project went well you can either keep your deduplicated
files in git-annex or `git annex unannex` them.

To be extra careful, you can also re-hash external folders later to confirm
that everything from them made it into the annex. Or re-hash the annex itself
to check file integrity with `git annex fsck`.

TODO: document interactive interface


[1]: https://git-annex.branchable.com
[2]: https://docs.haskellstack.org/en/stable/README/
[3]: https://nixos.org/nix
[4]: demo.sh
