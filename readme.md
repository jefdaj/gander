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

Use case
--------

Gander is intended as a last resort for when you have a very large number of
duplicate files that you can't afford to lose or mess up, but also can't look
through by hand.

It takes care to use properly: you need to plan out the operations you're going
to do beforehand and avoid touching the files with other programs at the same
time. But when you have so many files you can't even read all their filenames
quickly, there isn't any other option.

The end goal is to deduplicate your files to the point where you can use
regular git-annex operations on them, or `git annex unannex` them to get back
raw files.

Concepts
--------

Gander makes "scans" of your original + annexed files and relies on them for
searching and deduplicating. That's super useful when actually reading each
drive takes a long time (a couple days to a week in my case). The downside is
you need to make sure the scans are up to date. Don't edit your files between
doing a scan and using it to dedup! Scans are simple text files that list the
sha256sum + path of every file and folder. They extend the format used
[here][2] to deduplicate folders too.

Gander can scan and compare both annexed and non-annexed files, which lets you
verify that none of your data has been lost in the annex + dedup process.

Example
-------

Suppose you have a large number of old files spread over multiple hard drives.
They include a lot of duplicates because you went through several rounds of
"better back up everything up to be sure!", and eventually lost track of what
was what.

[1]: https://git-annex.branchable.com
