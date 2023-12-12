{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.List as L
import qualified System.Environment as Env
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map

-- Patterns for Data.Text
pattern x :> xs <- (Text.uncons -> Just (x, xs))
pattern Empty <- (Text.uncons -> Nothing)

-- Read lines from file
readLines :: String -> IO [Text.Text]
readLines f = fmap Text.lines (Text.readFile f)

-- Partial solution
data Partial = Partial {
  hd :: Text.Text,  -- processed text
  tl :: Text.Text,  -- remaining text to be processed
  curr :: Int,      -- length of current defective sequence we are at
  toPlace :: [Int]  -- sequences to be placed
} deriving (Show)

-- Parse line
parseLine :: Text.Text -> Partial
parseLine line = Partial { hd = Text.pack "", tl = log, curr = 0, toPlace = rle }
  where 
    [log, rest] = Text.split (' ' ==) line
    rle = map (read . Text.unpack) $ Text.split (',' ==) rest

-- Consume text with rewrite
shiftAs :: Char -> Partial -> Partial
shiftAs c partial@Partial{tl = x:>xs, ..} = partial{ hd = Text.snoc hd c, tl = xs }
shiftAs c partial@Partial{tl = Empty, ..} = error "Should not be called"

-- Extend solution by consuming another character from the remaining text
extendSolution :: Partial -> [Partial]
extendSolution partial@Partial{ curr = 0, toPlace = [], hd, tl = Empty } = [ partial ]
extendSolution partial@Partial{ curr = 0, toPlace = [], hd, tl = '.':>_ } = extendSolution $ shiftAs '.' partial
extendSolution partial@Partial{ curr = 0, toPlace = [], hd, tl = '?':>_ } = extendSolution $ shiftAs '.' partial
extendSolution partial@Partial{ curr = 0, tl = '.':>_ } = extendSolution $ shiftAs '.' partial
extendSolution partial@Partial{ curr = 0, tl = '#':>_ } = extendSolution $ (shiftAs '#' partial){curr = 1}
extendSolution partial@Partial{ curr = 0, tl = '?':>_ } = (extendSolution $ (shiftAs '#' partial){curr = 1}) ++ (extendSolution $ (shiftAs '.' partial){curr = 0})
extendSolution partial@Partial{ curr, toPlace = [p], tl = Empty } | curr == p = [ partial{curr = 0, toPlace = []} ]
extendSolution partial@Partial{ curr, toPlace = p:ps, tl = '#':>_ } | curr < p = extendSolution $ (shiftAs '#' partial){curr = curr + 1}
extendSolution partial@Partial{ curr, toPlace = p:ps, tl = '?':>_ } | curr < p = extendSolution $ (shiftAs '#' partial){curr = curr + 1}
extendSolution partial@Partial{ curr, toPlace = p:ps, tl = '.':>_ } | curr == p = extendSolution $ partial{curr = 0, toPlace = ps}
extendSolution partial@Partial{ curr, toPlace = p:ps, tl = '?':>_ } | curr == p = extendSolution $ (shiftAs '.' partial){curr = 0, toPlace = ps}
extendSolution Partial{} = []

main :: IO ()
main = do
  [filename] <- Env.getArgs
  values <- map ( length . extendSolution . parseLine) <$> readLines filename
  print $ sum values
