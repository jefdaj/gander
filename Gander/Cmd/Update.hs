moduel Gander.Cmd.Update where

-- TODO read old and new hash files (not dirs for now)
-- TODO determine path of new one inside old (error if not)
-- TODO replace old subdir with whole new tree
-- TODO write... to stdout? outfile?

-- TODO cmdUpdate :: Config -> FilePath -> FilePath -> FilePath -> IO ()
--      cmdUpdate cfg root sub path = do
--        tree1 <- buildTree (verbose cfg) (exclude cfg) root
--        tree2 <- buildTree (verbose cfg) (exclude cfg) sub
--        printHashes $ replaceSub tree1 tree2 path

-- TODO replaceSubTree :: HashTree -> HashTree -> FilePath -> HashTree
-- TODO replaceSubTree main sub path = ...
--      (this should work if the path doesn't exist yet too: just insert it)
