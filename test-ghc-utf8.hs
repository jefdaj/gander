import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import Util

import GHC.IO.Encoding (setLocaleEncoding)

main :: IO ()
main = do
  setLocaleEncoding System.IO.utf8

-- | Read a file as utf-8 (necessary for rpi)
readFile' :: FilePath -> IO T.Text
readFile' filepath = do
  handle <- openFile filepath ReadMode
  hSetEncoding handle utf8
  TIO.hGetContents handle

writeFile' :: FileName -> T.Text -> IO ()
writeFile' filepath txt = do
  handle <- openFile (n2p filepath) ReadMode
  hSetEncoding handle utf8
  TIO.hPutStr handle txt
