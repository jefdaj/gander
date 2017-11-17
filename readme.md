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
gander scan . > hashes.txt
head hashes.txt 
# 1a57975bd02fa70a39456759dddbd8aba500abc43419274757c1427ff6cdca44 file ./result/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0/usage.txt
# 0f2cf166f251d30db4766b9365e0a7aaef4598c6acf5599be8684161a9ec4cb1 dir  ./result/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0
# 202ad729f04ce319d7a717b3af88c3923f8fc90b98acb6cdd6eed61fda68a85f dir  ./result/share/x86_64-linux-ghc-8.0.2
# cde7f101bfbee08b37a693d9acd1450d0407ce605a69e36a77002163c912ec92 dir  ./result/share
# 97edf9c0f2a859bd105081ec832c35ae90af80a159e31ca96c3fde42b0011ca2 file ./result/bin/gander
# 71a3a3e4fedfa4c115fd85cffb6fbb5662c52166fdec2a15a0db6d86fb1f69b4 dir  ./result/bin
# 21371b2aa82ef0b50b76c8a9526486bd124fcf1276bf0eb6d7b5d40f25cebcd2 dir  ./result
# be6c8d4fb3493962dd201bf7238f15493ee18f1dd1c20bfde982f1fc8229145d file ./.stack-work/dist/x86_64-linux-nix/Cabal-1.24.2.0/stack-cabal-mod
# d4f45d26030dd68c566c44b69438e31f31b08b61500172dc4759a574408c5dd2 file ./.stack-work/dist/x86_64-linux-nix/Cabal-1.24.2.0/stack-config-cache
# f9645271ea7213b8203ef6686fb9af08ed33baa044a9c41c2f69a569184596c5 file ./.stack-work/dist/x86_64-linux-nix/Cabal-1.24.2.0/build/autogen/Paths_gander.hs

gander dupes hashes.txt > dupes.txt
head dupes.txt
# 3 duplicates:
# ./.stack-work/install/x86_64-linux-nix/lts-9.3/8.0.2/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0/usage.txt
# ./result/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0/usage.txt
# ./usage.txt
#
# 2 duplicates:
# ./.stack-work/install/x86_64-linux-nix/lts-9.3/8.0.2/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0
# ./result/share/x86_64-linux-ghc-8.0.2/gander-0.1.0.0
```

[1]: https://git-annex.branchable.com
