import Data.Monoid ((<>))
import Control.Applicative ((<$>),(<*>))
import System.Environment.XDG.BaseDir (getUserDataFile)
import Text.Read (readMaybe)
import System.IO.Error (catchIOError)

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

main :: IO ()
main = catchIOError (dataFile >>= readFile >>= return . read) (const $ return allNics) >>= print
