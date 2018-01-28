module Gander.Cmd.Dedup where

-- TODO explain to user:
--   list (first N) duplicates
--   what would you like to do?
--     skip for now
--     skip always (ignore)
--     save one of them by number
--     save to a new path you enter
--     quit
-- TODO "collapse" fn:
--   rm all but the one picked, double-checking they all still have the same hash
--   (error if not)
--   move the one picked if not at final destination
