import Data.Monoid ((<>))
import Control.Applicative ((<$>),(<*>))
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataFile, getUserDataDir)
import System.IO (hPutStr, withFile, IOMode(..))
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

vowel :: [Char]
vowel = "aoeui"

consanant :: [Char]
consanant = "bcdfghjklmnpqrstvwxyz"

listOfTwo :: a -> a -> [a]
listOfTwo a b = [a, b]

listOfThree :: a -> a -> a -> [a]
listOfThree a b c = [a, b, c]

allNics :: [String]
allNics = consanantDigraphs <> consanantTrigraphs
          <> vowelDigraphs <> vowelTrigraphs
  where consanantDigraphs = listOfTwo <$> consanant <*> vowel
        vowelDigraphs = listOfTwo <$> vowel <*> consanant
        consanantTrigraphs = listOfThree <$> consanant <*> vowel <*> consanant
        vowelTrigraphs = listOfThree <$> vowel <*> consanant <*> vowel

dataFile :: IO FilePath
dataFile = getUserDataFile "nic-gen" "current-nics"

dataDir :: IO FilePath
dataDir = getUserDataDir "nic-gen"

main :: IO ()
main = do name <- dataFile
          nics <- catchIOError (readFile name >>= readIO) $ const $ return allNics
          dataDir >>= createDirectoryIfMissing True
          withFile name WriteMode (flip hPutStr (show nics))
