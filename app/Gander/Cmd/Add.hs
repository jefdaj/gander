module Gander.Cmd.Add where

-- TODO is the dst path being duplicated a bit? like unsorted/u1/u1?

-- import Data.Gander
-- import Gander.Config (Config(..))
-- import Gander.Run    (safeRunDeltas, runRsync)

-- import Data.Maybe      (fromJust)
-- import System.FilePath ((</>))

-- TODO ensure that dst is a valid relative path, or trust the user?
-- cmdAdd :: Config -> FilePath -> Maybe FilePath -> IO ()
-- cmdAdd cfg dst mSrc = do
--   let aPath   = fromJust $ annex cfg
--       dstRoot = "unsorted" </> dst
--       dstAbs  = aPath </> dstRoot
--   dstTree <- case mSrc of
--     Nothing -> buildProdTree (verbose cfg) (exclude cfg) dstAbs
--     Just s  -> rsyncAndHash cfg s dstAbs -- TODO aha! safeRunDeltas doesn't take this into account
--     -- is there any need to --check when adding anyway? maybe need some extra logic for it
--   let ds  = [Add dstRoot dstTree]
--       msg = unwords ["gander add", dst] -- TODO sanitize!
--   safeRunDeltas cfg ds msg

-- rsyncAndHash :: Config -> FilePath -> FilePath -> IO (HashTree ())
-- rsyncAndHash cfg s dst' = do
--   _ <- runRsync cfg s dst' -- TODO control verbosity
--   before <- buildProdTree (verbose cfg) (exclude cfg) s
--   after  <- buildProdTree (verbose cfg) (exclude cfg) dst'
--   assertSameTrees ("original files ('" ++ s    ++ "')", before)
--                   ("annexed  files ('" ++ dst' ++ "')", after)
--   return after
