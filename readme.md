<img align="right" src="gander.png"></img>

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

The stack install has been tested on Mac OSX and Linux machines.
There are currently no plans for Windows support.

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
D 2 88bddd45f2a68576fdeebcef08c10101a568ab615747fbf4949e622fd57ad8d8 folder2
D 1 0abc967a6c1907a42360f0fc9e3695feddeb6dcafa5e1511afbae4e3be49669f folder1
D 0 bad85662b5cc09010d879c8f580f80125417a330ee7d5b5f4b70eee1ae7a95d2 backup
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


Annex-aware sorting
-------------------

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

The whole process would look something like this, assuming you have two drives
"thumbdrive" and "mybook" to deduplicate. First, start a repo.

```
$ gander myfirstrepo init
$ tree myfirstrepo
myfirstrepo/
├── hashes.txt
├── sorted
└── unsorted

2 directories, 1 file
```

Or use your own. It just needs to be a git-annex repo with `sorted` and
`unsorted` folders and a file `hashes.txt`.

Now add all your files. Each `add` command does a `git annex add`,
updates `hashes.txt` to include the new files, and `git commit`s.

```
$ gander myfirstrepo add /media/jefdaj/thumbdrive thumbdrive
$ gander myfirstrepo add /media/jefdaj/mybook     mybook
$ tree myfirstrepo/ | cut -d'-' -f1
myfirstrepo/
├── hashes.txt
├── sorted
└── unsorted
    ├── mybook
    │   ├── file1.txt
    │   ├── file2.txt
    │   └── file3.txt
    └── thumbdrive
        ├── file1.txt
        ├── file2.txt
        └── file3.txt

4 directories, 7 files
```

(The `cut` part hides messy symlinks from the output)

Finally run `gander myfirstrepo sort` and follow the prompts. It will loop
through all the duplicate groups, largest (most duplicates) first. It
will ask where in `sorted` you want each one saved. It moves one copy there,
removes the others, updates `hashes.txt` and commits after each step.

In this simple case the first move will clean up everything.
If you decide to name the sorted folder "files", it will look like:

```
$ gander myfirstrepo sort
$ tree | cut -d'-' -f1
myfirstrepo
├── hashes.txt
└── sorted
    └── files
        ├── file1.txt
        ├── file2.txt
        └── file3.txt

2 directories, 4 files
```

Annex safety
------------

Before each git operation, `gander` checks that the set of file hashes won't
change (aka no unique files will be lost), and that it isn't overwriting any
existing files. If it violates those rules, first of all make a bug report!
It's not supposed to. Then you can look in the `git log` and undo the offending
change:

```
commit 4adde98206cda56d94400a3b32ca47478f7a2859
Author: gander user <f@ke.email>
Date:   Mon Sep 10 17:06:05 2018 +0000

    gander sort: files

commit 60bcffbb45570226c15c2d64a32a2af85de94320
Author: gander user <f@ke.email>
Date:   Mon Sep 10 17:05:38 2018 +0000

    gander add mybook

commit 7046c79e21d8717b368eb8c9c3d5f6b21f756010
Author: gander user <f@ke.email>
Date:   Mon Sep 10 17:05:19 2018 +0000

    gander add thumbdrive

commit 062c941373c9d90798f5c18a4d81b6b510a9653b
Author: gander user <f@ke.email>
Date:   Mon Sep 10 17:04:35 2018 +0000

    gander init
```

When run on annexed files, `gander` never deletes the files themselves. Only
symlinks. Later you can run `git annex unused` to delete files with no links
pointing to them anymore. Until then, you can always get them back by
rewinding the git history to restore the links.

WARNING: If you `git mv`, `git annex add`, or `git rm` files ouside `gander`
make sure to run `gander myfirstrepo hash` to update `hashes.txt` to match
before continuing! `gander` assumes that file matches the current repository
state, and will give nonsensical results or possibly remove the last copy of
something if not.

There are two parts to the overall strategy in this mode:

1. Copy all the duplicate files to a cental [git-annex][1] repository and let
   git-annex deduplicate the file contents immediately by symlinking duplicate
   files to the same store path. That leaves a large number of directories +
   duplicate symlinks.

2. Use `gander` to iteratively deduplicate the directories and symlinks, committing after each step.
   If you mess up a step, the full history is still available in git.

Once satisfied that everything went according to plan you can either keep your
files in git-annex or `git annex unannex` them.


[1]: https://git-annex.branchable.com
[2]: https://docs.haskellstack.org/en/stable/README/
[3]: https://nixos.org/nix
[4]: demo.sh
