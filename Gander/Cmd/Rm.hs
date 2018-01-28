module Gander.Cmd.Rm where

-- TODO cmdRm :: Config -> FilePath -> FilePath -> IO ()
--      cmdRm cfg root sub = do
--        tree1 <- buildTree (verbose cfg) (exclude cfg) root
--        tree2 <- buildTree (verbose cfg) (exclude cfg) sub
--        guard1 <- safeToRm tree1 tree2
--        guard2 <- userConfirms "ok to remove last copy of ... ?"
--        when (guard1 || guard2) (rmDir root sub)

-- TODO safeToRm :: HashTree -> HashTree -> Bool
-- TODO userConfirms :: String -> IO Bool
-- TODO rmDir :: FilePath -> FilePath -> IO ()
-- TODO findAnnex :: FilePath -> IO (Maybe FilePath)
