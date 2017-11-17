gander
======

The "Git ANnex DedupER" deduplicates large numbers of files with git-annex.
Major design goals are:

1. Be sure nothing is deleted or lost accidentally
2. Be clear about what it will do before running commands
3. Be reasonably fast, given the limitations of git

Please note that those are only goals! It was written for a specific project
consisting of around 10 million files, and has not been tested beyond that.
There are no guarantees. Please send suggestions, pull requests, and/or report
whether it worked for you! Open to new use cases or syntax or whatever.

Examples
--------

Make a list of duplicate files in the repo itself.

```.bash
gander scan . > hashes.txt && gander dupes hashes.txt
# 3 duplicates:
# ./.stack-work/install/x86_64-linux-nix/lts-9.3/8.0.2/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0/usage.txt
# ./result/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0/usage.txt
# ./usage.txt
#
# 2 duplicates:
# ./.stack-work/install/x86_64-linux-nix/lts-9.3/8.0.2/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0
# ./result/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0
#
# 2 duplicates:
# ./.stack-work/dist/x86_64-linux-nix/Cabal-1.24.2.0/package.conf.inplace
# ./.stack-work/install/x86_64-linux-nix/lts-9.3/8.0.2/pkgdb
#
# 2 duplicates:
# ./.stack-work/install/x86_64-linux-nix/lts-9.3/8.0.2/share/x86_64-linux-ghc-8.0.2
# ./result/share/x86_64-linux-ghc-8.0.2
#
# ...
```

[1]: https://git-annex.branchable.com
