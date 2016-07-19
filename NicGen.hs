import Data.List (filter)
import Data.Monoid ((<>))
import Control.Applicative ((<$>),(<*>))
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserDataFile, getUserDataDir)
import System.IO (hPutStr, withFile, IOMode(..))
import System.IO.Error (catchIOError)
import System.Random (randomRIO)
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

readData :: IO [String]
readData = catchIOError (dataFile >>= readFile >>= readIO) $ const $ return allNics

writeData :: [String] -> IO ()
writeData nics = do dataDir >>= createDirectoryIfMissing True
                    name <- dataFile
                    withFile name WriteMode (flip hPutStr (show nics))

main :: IO ()
main = do nics <- readData
          nicToTry <- (nics !!) <$> randomRIO (0, length nics)
          putStrLn (nicToTry <> " (y|N)?")
          input <- (== "y") <$> getLine
          case input of
            False -> writeData $ filter (/= nicToTry) nics
            True -> writeData nics
          wrapper input
