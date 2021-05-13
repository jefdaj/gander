import Test.QuickCheck
import Test.QuickCheck.Instances
import Main.Utf8 (withUtf8)
import GHC.IO.Encoding (getLocaleEncoding)
-- import Util
import Control.Monad
import System.Directory
-- import System.FilePath

-- this looks super useful, but interestingly:
-- > on Windows or OSX platformFormat will be Text, and on Linux it will be ByteString.
import qualified Filesystem.Path.CurrentOS as OS

-- import qualified Data.ByteString.Unicode as BU
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = forM_ [1..10] $ \_ -> writeDiabolicalFile (T.pack "/tmp/diabolical")

writeDiabolicalFile :: T.Text -> IO T.Text
writeDiabolicalFile dir = do
  let (Right dir') = OS.toText dir
  putStrLn $ "dir': '" ++ dir' ++ "'"
  return dir
--   createDirectoryIfMissing True dir
--   basename <- fmap n2p $ generate (arbitrary :: Gen FileName)
--   let path = dir </> basename
--   putStr $ "writing diabolical file: "
--   putStr $ case OS.toText path of
--     Left approx -> "approximately '" ++ approx ++ "'"
--     Right exact -> "'" ++ exact ++ "'"
--   putStrLn ""
--   TIO.writeFile path "this is a test"
--   return path
