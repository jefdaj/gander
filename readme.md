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

```.bash
gander scan  /home/jefdaj/gander > hashes.txt
gander dupes /home/jefdaj/gander >  dupes.txt
```

[1]: https://git-annex.branchable.com
