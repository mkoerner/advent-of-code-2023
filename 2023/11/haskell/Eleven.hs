import Data.List
import qualified System.Environment as Env
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map

-- Generate map from old to new x values
expand :: Integer -> [Integer] -> [(Integer, Integer)]
expand expansionScale xs = scanl' f (start, start) rest
  where
    start = head xs
    rest = tail xs
    f (opos', npos') pos = (pos, expansionScale * (pos - opos' - 1) + npos' + 1)

-- Read lines from file
readLines :: String -> IO [Text.Text]
readLines f = fmap Text.lines (Text.readFile f)

-- Find character position in Text
charPos :: Char -> Text.Text -> [Integer]
charPos ref t = reverse . fst $ Text.foldl' locate ([],0) t
  where
    locate (ps, i) c = let next = i + 1 in (if c == ref then next:ps else ps, next)

-- Determine positions of galaxies
galaxyPos :: Char -> [Text.Text] -> [(Integer, Integer)]
galaxyPos gChar lines =
  concat $ map prod rPos
  where
    rPos = zip ([1..]) $ map (charPos gChar) lines
    prod (r,cs) = map (\c -> (r,c)) cs

-- Distance between two galaxies
distance :: (Integer,Integer) -> (Integer,Integer) -> Integer
distance (r,c) (r',c') = abs (r - r') + abs (c - c')

-- Generate all unique pairs
pairs :: [a] -> [(a,a)]
pairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

main :: IO ()
main = do
  [scale, filename] <- Env.getArgs
  lines <- readLines filename
  let
    expansionScale = read scale :: Integer
    positions = galaxyPos '#' lines
    rowPos = nub . sort $ map fst positions
    colPos = nub . sort $ map snd positions
    rowMap = Map.fromList (expand expansionScale rowPos)
    colMap = Map.fromList (expand expansionScale colPos)
    expanded = map (\(r,c) -> (rowMap Map.! r, colMap Map.! c)) positions
    result = sum $ map (\(l,r) -> distance l r) $ pairs expanded
  print result
