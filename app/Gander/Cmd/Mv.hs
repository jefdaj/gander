module Gander.Cmd.Mv where

-- TODO guess and check hashes
-- TODO next: fix relative paths thing, write a nice lost files warning, fix any last bugs... then good :D
-- TODO oh, write a couple other messages if it would help brian. lost files should be mentioned even when 0!

-- import Data.Gander
-- import Gander.Config (Config(..))
-- import Gander.Run    (safeRunDeltas)

-- TODO what if they mean to move something *inside* something that exists already?
--      that's ok but confusing here
-- TODO list files with no duplicates when confirming
-- TODO aha! ok to be missing folder hashes, just not files
-- cmdMv :: Config -> FilePath -> FilePath -> IO ()
-- cmdMv cfg src dst = do -- TODO correct toRm path using root!
--   let ds  = [Mv src dst]
--       msg = unwords ["mv", src, "->", dst] -- TODO sanitize!
--   safeRunDeltas cfg ds msg
