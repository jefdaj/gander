gander
======

The "Git ANnex DedupER" deduplicates files, optionally using [git-annex][1] to track changes.
It's especially suited for very large numbers of files that cause raw git operations to be slow.

There are two modes: simple for when you want to do a one-off operation like diff two folders,
and annex-aware for when you have a huge mess you want to clean up carefully.

Please send suggestions, pull requests, and/or report whether it worked for you!
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

If anyone shows interest I will also distribute precompiled versions. Email me or open an issue.


Save hashes
-----------

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

Using the hash command by itself is mainly useful when the files to hash are large and/or on an external drive.


Diff folders
------------

The advantage over `diff -r` is that you can use saved hashes to avoid re-scanning everything.
Detection of changes is also nicer, as shown in [demo.sh](demo.sh):

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

Either folder can be replaced by a file listing the corresponding hashes.
For example:

```
gander diff backup-hashes.txt current
```


Annex-aware mode
----------------

This is what `gander` was really designed for. Major goals are:

1. Be sure nothing is deleted or lost accidentally
2. Be clear about what it will do before running commands
3. Be reasonably fast, given the limitations of git

Please note that those are only goals! It was written for a specific project
consisting of around 10 million files, and has not been tested much beyond
that. However, that project included several complete Mac filesystems and a ton
of really horrendous filenames with emojis, newlines, unicode glyphs and
whatever else you can think of. So it made a decent stress test.


[1]: https://git-annex.branchable.com
[2]: https://docs.haskellstack.org/en/stable/README/
[3]: https://nixos.org/nix
