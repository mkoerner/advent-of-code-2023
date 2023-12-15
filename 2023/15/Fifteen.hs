import qualified System.Environment as Env
import Data.List.Split (splitOn)
import Data.Foldable (foldl')
import Data.Char (ord)

input :: IO [String]
input = lines <$> (readFile . head =<< Env.getArgs)

hash :: String -> Int
hash = foldl' (\h c -> (17 * (h + ord c)) `mod` 256) 0

hashLine :: String -> Int
hashLine = sum . map hash . splitOn ","

main :: IO ()
main = mapM_ print <$> map hashLine =<< input